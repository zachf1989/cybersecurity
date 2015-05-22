package capstone.gallery

import scala.collection.mutable.ArrayBuffer
import capstone.person.Person
import capstone.room.Room
import capstone.action.Action


class Gallery (val rooms: ArrayBuffer[Room], val people: ArrayBuffer[Person],
               var timestamp: Int = 0) extends Serializable
	{
	def this ()
		{
		this (new ArrayBuffer[Room], new ArrayBuffer[Person])
		}


    /* LOGREAD -S */
	def printStatus
		{
        val peopleByRole = people groupBy (_.isEmployee)
        val peopleByRoom = people groupBy (_.location)

		println (getNames (peopleByRole get true))
		println (getNames (peopleByRole get false))

		rooms.filter(_.population > 0).sortBy(_.id) foreach
            { x => println(x.id + ": " + getNames (peopleByRoom get x.id)) }
		}

        
    /* GET A LIST OF ALL THE PEOPLE IN THE GALLERY OR SPECIFIC ROOM */
	def getNames (peopleMap: Option[ArrayBuffer[Person]]) =
		{
        var names = new ArrayBuffer[String]

		if (peopleMap != None)
            {
            peopleMap.get foreach { x => if (x.location >= -1) names += x.name }
			}
            
        names.sorted mkString ", "
		}


    /* LOGREAD -R */
	def printVisitedRooms (name: String, isEmployee: Boolean)
		{
        val person = people find { tmp => tmp.name == name && tmp.isEmployee == isEmployee }
        val roomList = new ArrayBuffer[Int]

		if (person != None)
            person.get.actions foreach { tmp => if (tmp.roomId >= 0) roomList += tmp.roomId }

		println (roomList mkString ", ")
		}


    /* LOGREAD -T */
	def printTotalTime (name: String, isEmployee: Boolean)
		{
		val person = people find { tmp => tmp.name == name && tmp.isEmployee == isEmployee }
		var time = 0

		if (person != None)
			{
			val arrivals = person.get.actions filter (_.roomId == -1)
			val last = person.get.actions(person.get.actions.length - 1)

			arrivals foreach
				{
				x =>
				val start = x.aTime
				val end = if (x.lTime > 0) x.lTime
                          else if (last.lTime > 0) last.lTime
                          else timestamp

				time += (end - start)
                }
			}

		println(time)
		}


    /* LOGREAD -I*/
	def printSharedRooms (names: ArrayBuffer[Person])
		{
		val roomList = new ArrayBuffer[Int]

		if (names.length > 0)
			{
			var min = (-1, names(0))

			names.foreach
				{
				x =>
				val len = x.actions.length

				if (len < min._1 || min._1 == -1)
					min = (len, x)
				}

			val first = min._2

			names -= first

			first.actions foreach
				{
				x =>
                var allPresent = true
				val one = if (x.lTime >= 0) (x.aTime, x.lTime)
                          else (x.aTime, timestamp)

				names foreach { y => allPresent = allPresent && hasSharedRoom (x.roomId, one, y) }

				if (allPresent && x.roomId >= 0)
					roomList += x.roomId
				}
			}

		println(roomList.mkString(", "))
		}

    
    /* DOES THE PERSONS SHARE A ROOM */
	def hasSharedRoom (roomId: Int, one: (Int, Int), person: Person) =
		{
		var status = false

		person.actions.foreach
			{
			x =>
			val temp = if (x.roomId == roomId && roomId >= 0)
                            {
                            val two = if (x.lTime >= 0)
                                        (x.aTime, x.lTime)
						              else
                                        (x.aTime, timestamp)
                    
					        isSharedTime (one, two)
					        }
                       else false

            status = status || temp
			}

		status
		}


    /* ARE THE TWO TIMES INTERSECTING */
	def isSharedTime (one: (Int, Int), two: (Int, Int)) =
		{
		(two._1 <= one._2 && two._1 >= one._1) ||
		(one._1 <= two._2 && one._1 >= two._1)
		}


    /* LOGAPPEND -L */
	def leaveLocation (timestamp: Int, name: String, isEmployee: Boolean, roomId: Int) =
		{
		val person = people find { tmp => tmp.name == name && tmp.isEmployee == isEmployee }

        /* IF PERSON EXISTS */
		if (person != None)
            {
            /* PERSON IS IN THE ROOM THAT THEY ARE TRYING TO LEAVE */
            if (roomId == person.get.location)
                {
				val room = rooms find (_.id == roomId)

                if (room != None)
				    room.get.population -= 1

				person.get.location = if (roomId >= 0) -1
                                      else -2

				val action = person.get.actions find (_.lTime == person.get.location)
				action.get.lTime = timestamp
				this.timestamp = timestamp
				0
				}
            else 255
            }
        else 255
		}


    /* LOGAPPEND -A */
	def enterLocation (timestamp: Int, name: String, isEmployee: Boolean, roomId: Int) =
		{
		var status = 255
		var person = people find { x => x.name == name && x.isEmployee == isEmployee }

		/* PERSON IS ENTERING A ROOM */
		if (roomId >= 0 && roomId < 1073741824 && person != None)
			{
            /* CHECK IF THE PERSON IS NOT IN ANOTHER ROOM */
			if (person.get.location == -1)
				{
				val room = rooms find (_.id == roomId)

                /* ADD PERSON IF ROOM EXISTS, ELSE CREATE A NEW ROOM */
				if (room != None)
					room.get.population += 1
				else
				    rooms += new Room (roomId)

				person.get.location = roomId
				this.timestamp = timestamp

				person.get.actions += new Action (roomId, timestamp, -1)

				status = 0
				}
			}

		/* PERSON IS ENTERING THE GALLERY */
		else if (roomId == -1)
			{
            /* IF PERSON DOESNT EXIST, CREATE A NEW PERSON */
			if (person == None && name != "")
                		{
				        person = Option (new Person (name, isEmployee))
				        people += person.get
				        }

            /* IF PERSON EXISTS, LET THEM ENTER THE GALLERY */
			if (person != None)
				{
				if (person.get.location == -2)
					{
					person.get.location = -1
					this.timestamp = timestamp
					person.get.actions += new Action (roomId, timestamp, -2)
					status = 0
					}
				}
			}

		status
		}
	}