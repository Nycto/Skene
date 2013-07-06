package com.roundeights.skene.static

import com.roundeights.skene.{Request, Response, Recover, Handler}
import java.io.{File, IOException}
import scala.concurrent.ExecutionContext

/**
 * Dispenses an asset to the client
 */
class AssetHandler
    ( private val finder: (Asset) => Option[Asset.Reader] )
    ( implicit context: ExecutionContext )
extends Handler {

    /** {@inheritDoc} */
    override def toString = "AssetHandler(%s)".format(finder)

    /** Finds an asset from the URL in a request */
    private def find ( req: Request ): Option[Asset.Reader] = {
        req.params.get("asset")
            .orElse( req.url.path )
            .map( Asset(_) )
            .flatMap( asset => finder(asset.unversioned).orElse(finder(asset)) )
    }

    /** Determines if a resource is in the client's cache */
    private def isInCache( request: Request, asset: Asset.Reader ): Boolean = {
        request.getDateHeader("If-Modified-Since").map(
            cached => asset.modified.getTime <= cached.getTime
        ).getOrElse( false )
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

            case Some(reader) if isInCache( request, reader ) => {
                response.code( Response.Code.NotModified() )
                response.done
            }

            case Some(reader) => {
                response.header(
                    Response.Header.CacheControl(),
                    "max-age=31560000, must-revalidate, public"
                )
                response.header(Response.Header.LastModified(), reader.modified)
                reader.mimeType.map( mime => response.contentType(mime) )
                response.content( reader.renderable )
                response.done
            }
        }
    }

}


