package capstone.security

import javax.crypto.Cipher
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.SecretKeySpec
import javax.crypto.spec.PBEKeySpec
import javax.crypto.SealedObject
import capstone.gallery.Gallery

object Security
	{
    /* TURN A TOKEN STRING INTO A VALID AES KEY */
	def generateToken (token: String) = 
		{
		val factory = SecretKeyFactory getInstance "PBKDF2WithHmacSHA1"
		val spec = new PBEKeySpec (token.toCharArray, new String("1234").getBytes, 10, 128)
		val tmp = factory generateSecret spec
		new SecretKeySpec (tmp.getEncoded, "AES")
		}


    /* ENCRYPT THE GALLERY */
	def sealGallery (obj: Gallery, key: SecretKeySpec) =
		{
		val cipher = Cipher getInstance "AES/CBC/PKCS5Padding"
		cipher init (Cipher.ENCRYPT_MODE, key)
		new SealedObject (obj, cipher)
		}


    /* DECRYPT AND VERIFY LOG FILE */
	def verifyGallery (obj: SealedObject, key: SecretKeySpec) =
		{
		try { Some(obj.getObject(key).asInstanceOf[Gallery]) }
		catch { case e: Exception => None }
		}
	}