package capstone.security

import javax.crypto.Cipher
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.SecretKeySpec
import javax.crypto.spec.PBEKeySpec
import javax.crypto.SealedObject
import capstone.gallery.Gallery


object Security
	{        
    def generateToken(token: String) = 
		{
		val factory = SecretKeyFactory getInstance "PBKDF2WithHmacSHA1"
		val spec = new PBEKeySpec(token.toCharArray, new String("1234").getBytes, 65536, 256)
		val tmp = factory generateSecret spec
		new SecretKeySpec(tmp.getEncoded, "AES")
		}


	def sealGallery(obj: Gallery, key: SecretKeySpec) =
		{
		val cipher = Cipher getInstance "AES/CBC/PKCS5Padding"
		cipher.init(Cipher.ENCRYPT_MODE, key)

		new SealedObject(obj, cipher)
		}


	def verifyGallery(obj: SealedObject, key: SecretKeySpec) =
		{
		var gallery: Option[Gallery] = None

        try { gallery = Some(obj.getObject(key).asInstanceOf[Gallery]) }
        catch { case e: Exception => gallery = None }

		gallery
		}
	}
