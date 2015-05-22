import java.io.File
import capstone.gallery.Gallery
import capstone.security.Security
import capstone.io.IO


object logread
	{
	def main (args: Array[String])
		{
		val log = IO validCommand (args, false)

		val status = if (log != None)
				{
				val logFile = new File (log.get)
				val token = IO getArg (args, "-K")
				val secretToken = Security generateToken token
				val gallery = IO readGallery (logFile, secretToken)
                                    
				if (gallery != None)
					{
					if (printGallery (gallery.get, args) != 0)
						{
                        /* INVALID COMMAND */
						println("invalid")
						255
						}
					else 0
					}

				else
					{
                    /* CANNOT VERIFY THE INTEGRITY OF THE LOG FILE */
					println("integrity violation")
					255
					}
				}
			else
				{
                /* LOG FILE DOESNT EXIST */
				println("invalid")
				255
				}

		System exit status
		}


	def printGallery (obj: Gallery, args: Array[String]) =
		{
		val (hasS, hasR, hasT, hasI, hasE, hasG) =
			(args contains "-S", args contains "-R", args contains "-T", args contains "-I", 
			 args contains "-E", args contains "-G")

		if ((hasS && !hasG && !hasE) || ((hasR || hasT || hasI) && (hasG || hasE)))
			{
			if (hasS && !hasR && !hasT && !hasI)
				obj.printStatus
			else if (hasR || hasT || hasI)
				{
				val name = if (hasE && !hasG)
				                IO getArg (args, "-E")
                           else if (hasG && !hasE)
                                IO getArg (args, "-G")
                           else
                                new String

				if (hasR && !hasT && !hasS && !hasI)
					obj printVisitedRooms (name, hasE)
				else if (hasT && !hasS && !hasR && !hasI)
					obj printTotalTime (name, hasE)
				else if (hasI && !hasT && !hasS && !hasR)
					obj printSharedRooms (IO getPeople (obj, args))
				}
            0
			}

		else
			255
		}
	}