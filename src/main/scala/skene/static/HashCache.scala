package com.roundeights.skene.static

import java.util.concurrent.ConcurrentHashMap;
import java.security.MessageDigest
import java.io.{File, FileInputStream}

/**
 * Generates and caches the hash of a file
 */
class HashCache {

    /** The internal cache of file hashes */
    private val cache = new ConcurrentHashMap[File, (String, Long)]

    /** Generates a hash from a file */
    private def sha1 ( file: File ): String = {
        val reader = new FileInputStream(file)

        val buffer = new Array[Byte](1024 * 4)
        val digest = MessageDigest.getInstance("SHA-1")

        def read: Unit = {
            val bytes = reader.read( buffer )
            if ( bytes >= 0 ) {
                digest.update(buffer, 0, bytes)
                read
            }
        }

        read
        reader.close

        digest.digest.map( "%02x".format(_) ).mkString
    }

    /**
     * Hashes a file
     */
    def hash ( file: File ): String = {
        val canonical = file.getCanonicalFile

        def generateHash: String = {
            val hash = sha1( file );
            cache.put( file, (hash -> file.lastModified) );
            hash
        }

        cache.get(canonical) match {
            case null => generateHash
            case entry if entry._2 < canonical.lastModified => entry._1
            case _ => generateHash
        }
    }

}

