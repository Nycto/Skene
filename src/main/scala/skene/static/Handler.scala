package com.roundeights.skene.static

import com.roundeights.skene.{Request, Response, Recover, Handler}
import java.io.{File, IOException}
import scala.concurrent.ExecutionContext

/**
 * Dispenses an asset to the client
 */
class AssetHandler (
    private val finder: (String) => Option[Asset],
    private val asString: Option[String] = None
)(
    implicit context: ExecutionContext
) extends Handler {

    /** Constructs a new instance that searches for assets in a root dir */
    def this ( root: File )( implicit context: ExecutionContext ) = this(
        (path: String) => {
            val asset = Asset( root, path )
            if ( asset.exists ) Some(asset) else None
        },
        Some(root.toString)
    )

    /** {@inheritDoc} */
    override def toString = "AssetHandler(%s)".format( asString.getOrElse("") )

    /** Removes the version from a path */
    private def unversion ( path: String ) = {
        val base = Asset.stripExt( Asset.stripExt( path ) )
        Asset.ext( path ).map( base + _ ).getOrElse( base )
    }

    /** Finds an asset from the URL in a request */
    private def find ( req: Request ): Option[Asset] = {
        req.params.get("asset")
            .orElse( req.url.path )
            .map( Asset.canonicalize _ )
            .flatMap( path => finder(unversion(path)).orElse(finder(path)) )
    }

    /** Determines if a resource is in the client's cache */
    private def isInCache( request: Request, asset: Asset ): Boolean = {
        request.getDateHeader("If-Modified-Since").flatMap( cached => {
            asset.modified.map( _.getTime <= cached.getTime )
        }).getOrElse( false )
    }

    /** {@inheritDoc} */
    override def handle(
        recover: Recover, request: Request, response: Response
    ): Unit = {

        find( request ) match {
            case None => {
                response.notFound
                response.done
            }

            case Some(asset) if isInCache( request, asset ) => {
                response.code( Response.Code.NotModified() )
                response.done
            }

            case Some(asset) => {
                response.header(
                    Response.Header.CacheControl(),
                    "max-age=31560000, must-revalidate, public"
                )
                asset.modified.map( modified => {
                    response.header(Response.Header.LastModified(), modified)
                })
                asset.mimeType.map( mime => response.contentType(mime) )
                response.content( asset.renderable )
                response.done
            }
        }
    }

}


