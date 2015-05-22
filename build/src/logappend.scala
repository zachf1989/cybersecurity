import java.io.File
import javax.crypto.spec.SecretKeySpec
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import capstone.gallery.Gallery
import capstone.security.Security
import capstone.io.IO


object logappend
	{
	def main(args: Array[String])
		{
		val status = if (IO.isValidBatch(args))
				processBatchFile(args)
			     else
				{
				val log = IO.validCommand(args, true)

				if (log != None)
					{
					val logFile = new File(log.get)
					val token = IO.getArg(args, "-K")
                            		val secretToken = Security generateToken token
                            		val gallery = IO.readGallery(logFile, secretToken)
                    
                            		if (gallery != None)
				        	if (editGallery(gallery.get, args) == 0)
                                    			IO.writeGallery(gallery.get, logFile, secretToken)
						else 255
					else 255
					}

				else 255
				}

		if (status == 255)
			println("invalid")

		System.exit(status)
		}


	def processBatchFile(args: Array[String]) =
		{
		try
			{
			val file = new File(IO.getArg(args, "-B"))
			val data = new ArrayBuffer[(String, String, SecretKeySpec, Gallery)]
			Source.fromFile(file).getLines.foreach { x => processBatchCommand(x.split("\\s+"), data) }
            		data.foreach { x => IO.writeGallery(x._4, new File(x._1), x._3) }
            		0
            		}
		catch { case e: Exception => 255 }
		}


	def processBatchCommand(args: Array[String], data: ArrayBuffer[(String, String, SecretKeySpec, Gallery)]) =
		{
		var status = 255
		val log = IO.validCommand(args, true)
		val token = IO.getArg(args, "-K")

		if (log != None)
			{
			val item = data.find(x => x._1 == log.get && x._2 == token)

			status = if (item != None)
					editGallery(item.get._4, args)
				 else
					addGallery(args, log.get, token, data)
			}

		if (status == 255)
			println("invalid")
		}


	def addGallery(args: Array[String], log: String, token: String, data: ArrayBuffer[(String, String, SecretKeySpec, Gallery)]) =
		{
		val secretToken = Security generateToken token
		val logFile = new File(log)
		val gallery = IO.readGallery(logFile, secretToken)

		val status = if (gallery != None)
                		editGallery(gallery.get, args)
			     else
				255

		if (status == 0)
			{
			val datum = (log, token, secretToken, gallery.get)
			data += datum
			}

		status
		}


	def editGallery(obj: Gallery, args: Array[String]) =
		{
		var status = 255
		val (hasK, hasT, hasE, hasG, hasA, hasL, hasR) =
			(args contains "-K", args contains "-T", args contains "-E", args contains "-G", 
			 args contains "-A", args contains "-L", args contains "-R")

		val timestamp = IO.getInt(IO.getArg(args, "-T"))

		if (timestamp > obj.timestamp && hasK && (hasE || hasG) && (hasA || hasL))
			{
			val room = IO.getInt(IO.getArg(args, "-R"))

			/* EMPLOYEE OR GUEST NAME */
			val name = if (hasE && !hasG)
					IO.getArg(args, "-E")
				   else if (hasG && !hasE)
					IO.getArg(args, "-G")
				   else
					new String

			/* ENTERING OR LEAVING */
			status = if (hasA && !hasL)
					obj.enterLocation(timestamp, name, hasE, room)
				 else if (hasL && !hasA)
					obj.leaveLocation(timestamp, name, hasE, room)
				 else
					255
			}

		status
		}
	}
