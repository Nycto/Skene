package com.roundeights.skene.static

import scala.concurrent.ExecutionContext
import java.io.File
import scala.xml.Utility
import com.roundeights.skene.{Matcher, Request, Handler}

/** Builds the URLs and HTML needed to load an asset */
class AssetLoader (
    pathPrefix: String,
    private val hash: HashCache,
    private val finder: (String) => Option[Asset],
    private val asString: Option[String] = None
)(
    implicit context: ExecutionContext
) {

    /** The prefix to attach to each asset */
    private val prefix = {
        val canonical = Asset.canonicalize( pathPrefix )
        if ( canonical == "" ) None else Some(canonical)
    }

    /** Constructs a new instance that searches for assets in a root dir */
    def this
        ( root: File, prefix: String )
        ( implicit context: ExecutionContext )
    = this(
        prefix,
        HashCache(),
        (path: String) => {
            val asset = Asset( root, path )
            if ( asset.exists ) Some(asset) else None
        },
        Some( root.toString + ", " + prefix )
    )

    /** Returns a companion Handler */
    def handler: Handler with Matcher = {
        Matcher.and(
            Matcher.method( Request.Method.GET() ),
            Matcher.path( prefix.map( "/" + _ ).getOrElse("") + "/::asset" )
        ).handle(
            new AssetHandler( finder, asString )
        )
    }

    /** Returns the relative URL for an asset */
    def url ( path: String ): Option[String] = {
        for {
            asset <- finder( Asset.canonicalize(path) )
            sha1 <- hash.hash(asset.file)
        } yield "%s/%s.%s%s".format(
            prefix.map( "/" + _ ).getOrElse(""),
            asset.stripExt,
            sha1.take(8),
            asset.ext.getOrElse("")
        )
    }

    /** Formats a list of assets */
    private def html ( files: Seq[String], html: String ): String = {
        files.map( file => (file -> url(file)) ).map( _ match {
            case (file, None) => "<!-- Asset not found: %s -->".format(file)
            case (_, Some(path)) => html.format( Utility.escape(path) )
        }).mkString("\n")
    }

    /** Returns the HTML needed to load a list of CSS assets. */
    def css ( files: String* ): String
        = html(files, "<link rel='stylesheet' href='%s' type='text/css'>")

    /** Returns the HTML needed to load a list of JS assets. */
    def js ( files: String* ): String
        = html(files, "<script type='text/javascript' src='%s'></script>")

}

