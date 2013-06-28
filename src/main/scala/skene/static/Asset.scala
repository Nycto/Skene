package com.roundeights.skene.static

import com.roundeights.skene.Response.ContentType
import com.roundeights.skene.Renderable
import java.io.File
import java.util.Date

/** @see Asset */
object Asset {

    /** Creates an asset from a root string and path */
    def apply ( root: String, path: String ) = new Asset(new File(root), path)

    /** Creates an asset from a root and path */
    def apply ( root: File, path: String ) = new Asset(root, path)

    /** Returns the extension */
    private[static] def ext( path: String ): Option[String] = {
        path.lastIndexOf('.') match {
            case -1 => None
            case 0 => None
            case dot => path.lastIndexOf('/') match {
                case slash if slash >= 0 && slash > dot - 2 => None
                case _ => Some( path.drop(dot) )
            }
        }
    }

    /** Returns the path without the extension */
    private[static] def stripExt( path: String ): String = {
        ext( path ).map( _.length ) match {
            case None => path
            case Some(length) => path.dropRight(length)
        }
    }

    /** Returns a canonical path */
    private[static] def canonicalize( path: String ): String = {
        path.split( File.separator ).foldLeft( List[String]() )( {
            (stack, dir) => dir match {
                case "" => stack
                case "." => stack
                case ".." => stack.drop(1)
                case _ => dir :: stack
            }
        }).reverse.mkString( File.separator )
    }

}

/**
 * Represents an asset to serve back
 */
class Asset( val root: File, rawPath: String ) {

    /** The cleaned up path */
    val path = Asset.canonicalize( rawPath )

    /** {@inheritDoc} */
    override def toString = "Asset(%s, %s)".format(root, path)

    /** Returns the extension */
    def ext: Option[String] = Asset.ext( path )

    /** Returns the path without the extension */
    def stripExt: String = Asset.stripExt( path )

    /** Returns the path of this Asset with a version embedded */
    def versioned ( version: String ): String = {
        val base = "%s.%s".format( stripExt, version )
        ext.map( base + _ ).getOrElse( base )
    }

    /** Returns the mime type of this asset */
    def mimeType: Option[ContentType] = {
        ext.flatMap( _.toLowerCase match {
            case ".bmp"  => Some( ContentType.Bmp() )
            case ".css"  => Some( ContentType.Css() )
            case ".gif"  => Some( ContentType.Gif() )
            case ".htm"  => Some( ContentType.Html() )
            case ".html" => Some( ContentType.Html() )
            case ".ico"  => Some( ContentType.Icon() )
            case ".jpg"  => Some( ContentType.Jpeg() )
            case ".jpeg" => Some( ContentType.Jpeg() )
            case ".js"   => Some( ContentType.JavaScript() )
            case ".json" => Some( ContentType.Json() )
            case ".pdf"  => Some( ContentType.Pdf() )
            case ".png"  => Some( ContentType.Png() )
            case ".svg"  => Some( ContentType.Svg() )
            case ".swf"  => Some( ContentType.Swf() )
            case ".txt"  => Some( ContentType.Text() )
            case ".tiff" => Some( ContentType.Tiff() )
            case ".xml"  => Some( ContentType.Xml() )
            case ".xslt" => Some( ContentType.Xslt() )
            case ".zip"  => Some( ContentType.Zip() )
            case _ => None
        })
    }

    /** Returns a full file representing this asset */
    def file: File = new File( root, path )

    /** Returns a renderable representation of this asset */
    def renderable: Renderable = Renderable( file )

    /** Returns the modificaion time of this file */
    def modified: Option[Date] = {
        file match {
            case asset if asset.exists => Some( new Date(asset.lastModified) )
            case _ => None
        }
    }

    /** Returns whether this asset exists, is a file, and is readable */
    def exists: Boolean = {
        val path = file
        path.exists && path.isFile && path.canRead
    }

}

