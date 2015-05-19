package capstone.gallery

import scala.collection.mutable.ArrayBuffer
import capstone.person.Person
import capstone.room.Room


class Gallery(val rooms: ArrayBuffer[Room], val people: ArrayBuffer[Person], var timestamp: Int) extends Serializable
	{
	def this()
		{
		this(new ArrayBuffer[Room], new ArrayBuffer[Person], 0)
		}

	def printStatus() =
		{
        val status = 0
        val peopleByRole = people groupBy { x => x.isEmployee }

		println(getNames(peopleByRole.get(true)))
		println(getNames(peopleByRole.get(false)))
		
		val peopleByRoom = people groupBy { x => x.location }

        rooms.filter(_.population > 0).sortBy(_.id).foreach
            { x => println(x.id + ": " + getNames(peopleByRoom.get(x.id))) }
            
        status
        }


	def getNames(peopleMap: Option[ArrayBuffer[Person]]) =
		{
		if (peopleMap != None)
			{
			var names = new ArrayBuffer[String]
			peopleMap.get.foreach(x => if (x.location >= -1) names += x.name)
			names.sorted mkString ", "
			}
		else
			new String
		}


	def printVisitedRooms(name: String) =
		{
        val person = people find { x => x.name == name }
	
        val rooms = if (person != None)
                        {
                        var roomList = new ArrayBuffer[Int]
                        person.get.actions.foreach {x => if (x._2 >= 0) roomList += x._2}
                        roomList.distinct.sorted.mkString(", ")
                        }
                    else new String

        println(rooms)
        0
		}


	def printTotalTime(name: String) =
		{
        val person = people find { x => x.name == name }

        val time = if (person != None)
                        {
                        val roomList = new ArrayBuffer[Int]
                        person.get.actions.foreach(x => roomList += x._1)
                        roomList.max - roomList.min
                        }
                    else 0

        println(time)
        0
        }


    def printSharedRooms() =
        {
        println("unimplemented")
        0
        }


	def leaveLocation(timestamp: Int, name: String, isEmployee: Boolean, roomId: Int) =
		{
		var status = 255
		val person = people find { x => x.name == name }

		if (person != None)
			{
			if (roomId == person.get.location && isEmployee == person.get.isEmployee)
				{
                val room = rooms.find(_.id == roomId)
                
                if (room != None)
                    room.get.population -= 1
            
				person.get.location = if (roomId >= 0)
                                        -1
						              else
                                        -2

                val action = (timestamp, roomId)
				person.get.actions += action
				this.timestamp = timestamp
				status = 0
				}
			}

		status
		}


	def enterLocation(timestamp: Int, name: String, isEmployee: Boolean, roomId: Int) =
		{
		var status = 255
		var person = people find { x => x.name == name }

        /* PERSON IS ENTERING A ROOM */
		if (roomId >= 0 && person != None)
            {
            if (person.get.location == -1 && person.get.isEmployee == isEmployee)
				{
                val room = rooms find { x => x.id == roomId }

				if (room != None)
                    room.get.population += 1
                else
				    rooms += new Room(roomId)

				person.get.location = roomId
                this.timestamp = timestamp
                val action = (timestamp, roomId)
                person.get.actions += action

				status = 0
				}
			 }
        
        /* PERSON IS ENTERING THE GALLERY */
		else if (roomId == -1)
			{
			if (person == None)
                {
				person = Option(new Person(name, isEmployee))
                people += person.get
                }

			if (person != None)
				{
				if (person.get.location == -2 && person.get.isEmployee == isEmployee)
					{
					person.get.location = -1
                    this.timestamp = timestamp
                    val action = (timestamp, roomId)
					person.get.actions += action
                
					status = 0
					}
				}
			}

        status
		}
	}
