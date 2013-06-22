package com.roundeights.skene.static

import java.io.File

/** @See Asset */
object Asset {

    /** Creates an asset from a root string and path */
    def apply ( root: String, path: String ) = new Asset(new File(root), path)
}

/**
 * Represents an asset to serve back
 */
case class Asset( val root: File, val path: String ) {

    /** Returns the extension */
    def ext: Option[String] = {
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
    def stripExt: String = {
        ext.map( _.length ) match {
            case None => path
            case Some(length) => path.dropRight(length)
        }
    }

    /** Returns the path of this Asset with a version embedded */
    def versioned ( version: String ): String = {
        val base = "%s.%s".format( stripExt, version )
        ext.map( base + _ ).getOrElse( base )
    }

}

