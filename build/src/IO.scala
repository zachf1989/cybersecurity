package capstone.io

import java.security.KeyPair
import javax.crypto.SealedObject
import javax.crypto.spec.SecretKeySpec
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import scala.collection.mutable.ArrayBuffer
import capstone.gallery.Gallery
import capstone.security.Security
import capstone.person.Person


object IO
	{
	def getArg(args: Array[String], key: String) =
		{
		var obj = new String
		var matched = false

		args.foreach
			{
			i => if (i == key)
				matched = true
			     else if (matched)
				{
				obj = i
                    		matched = false
                    		}
			}

		obj
		}


	def getPeople(obj: Gallery, args: Array[String]) =
		{
		val names = new ArrayBuffer[Person]
		var matched = false
		var isEmployee = false

		args.foreach
			{
			x => if (matched)
				{
				val person = obj.people.find { y => y.name == x && y.isEmployee == isEmployee }

				if (person != None)
					names += person.get

				matched = false
				}
			     else if (x == "-E" || x == "-G")
				{
				isEmployee = x == "-E"
				matched = true
				}
			}

		names
		}


	def isValidBatch(args: Array[String]) =
		{
		var (result, index) = (0, 0)

		while (index < args.length && result >= 0)
			{
			result = -1

			if (index < args.length -1)
				if (args(index) == "-B" &&
				    args(index+1).matches("^[a-zA-Z0-9_./]+$"))
					result = 1

			index += 2
			}

		result == 1
		}


	def validCommand(args: Array[String], isAppend: Boolean) =
		{
		var (result, logs, index, path) = (0, 0, 0, new String)

		while (index < args.length && result >= 0)
			{
			result = if (isAppend)
					isAppendOption(args, index)
				 else
					isReadOption(args, index)

			if (result == 0)
				{
				logs += 1
				path = args(index)
				}
			else
				index += result-1

			if (logs == 2)
				result = -1

			index += 1
			}

		if (result >= 0 && logs == 1)
			Some(path)
		else
			None
		}


	def isAppendOption(args: Array[String], index: Int) =
		{
		val arg = args(index)

		if (arg.matches("^[a-zA-Z0-9_./]+$")) 0
		else if (arg == "-A" || arg == "-L") 1
		else if ((index < args.length - 1) &&
			 ((arg == "-B" && args(index+1).matches("^[a-zA-Z0-9]+$")) ||
			 (arg == "-K" && args(index+1).matches("^[a-zA-Z0-9]+.*$")) ||
			 ((arg == "-E" || arg == "-G") && args(index+1).matches("^[a-zA-Z]+.*$")) ||
			 ((arg == "-T" || arg == "-R") && args(index+1).matches("^[0-9]+$")))) 2

		else -1
		}


	def isReadOption(args: Array[String], index: Int) =
		{
		val arg = args(index)

		if (arg.matches("^[a-zA-Z0-9_./]+$")) 0
		else if (arg == "-S" || arg == "-R" || arg == "-T" || arg == "-I") 1
		else if ((index < args.length - 1) &&
			 ((arg == "-K" && args(index+1).matches("^[a-zA-Z0-9]+.*$")) ||
			 ((arg == "-E" || arg == "-G") && args(index+1).matches("^[a-zA-Z]+.*$")))) 2

		else -1
		}


	def getInt(arg: String) =
		{
		try { arg.toInt }
		catch { case e: Exception => -1 }
		}


	def readGallery(file: File, token: SecretKeySpec) =
		{
		if (file.exists)
			{
			val sealedObj = try
						{
						val oin = new ObjectInputStream(new FileInputStream(file))
						val obj = oin.readObject
						oin.close

						Some(obj.asInstanceOf[SealedObject])
						}

					catch { case e: Exception => None }

			if (sealedObj != None)
				Security.verifyGallery(sealedObj.get, token)
			else
				None
			}

		else
			Some(new Gallery)
		}

        
	def writeGallery(obj: Gallery, file: File, token: SecretKeySpec) =
		{
		val sealedObj = Security.sealGallery(obj, token)

		try
			{
			val oout = new ObjectOutputStream(new FileOutputStream(file))

			oout writeObject sealedObj
			oout.close
			0
			}
		catch { case e: Exception => 255 }
		}
	}
