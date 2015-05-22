package capstone.gallery

import scala.collection.mutable.ArrayBuffer
import capstone.person.Person
import capstone.room.Room
import capstone.action.Action


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


	def printVisitedRooms(name: String, isEmployee: Boolean) =
		{
		val person = people find { x => x.name == name && x.isEmployee == isEmployee }

		val rooms = if (person != None)
				{
				var roomList = new ArrayBuffer[Int]
				person.get.actions.foreach {x => if (x.roomId >= 0) roomList += x.roomId}
				roomList.mkString(", ")
				}
			    else new String

		println(rooms)
		0
		}


	def printTotalTime(name: String, isEmployee: Boolean) =
		{
		val person = people find { x => x.name == name && x.isEmployee == isEmployee }
		var time = 0

		if (person != None)
			{
			val arrivals = person.get.actions.filter(_.roomId == -1)
			val last = person.get.actions(person.get.actions.length-1)

			arrivals.foreach
				{
				x =>
				val start = x.aTime
				val end = if (x.lTime > 0)
						x.lTime
					  else if (last.lTime > 0)
						last.lTime
					  else
						last.aTime

				time += (end - start)
                       		}
			}

		println(time)
		0
		}


	def printSharedRooms(names: ArrayBuffer[Person]) =
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

			first.actions.foreach
				{
				x =>
				val one = if (x.lTime >= 0)
						(x.aTime, x.lTime)
					  else
						(x.aTime, timestamp)

				var allPresent = true

				names.foreach
					{
					y =>
					allPresent = allPresent &&
						     hasSharedRoom(x.roomId, one, y)
					}

				if (allPresent)
					roomList += x.roomId
				}
			}

		println(roomList.mkString(", "))
		0
		}


	def hasSharedRoom(roomId: Int, one: (Int, Int), person: Person) =
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

					isSharedTime(one, two)
					}
				   else false

			status = status || temp
			}

		status
		}


	def isSharedTime(one: (Int, Int), two: (Int, Int)) =
		{
		(two._1 <= one._2 && two._1 >= one._1) ||
		(one._1 <= two._2 && one._1 >= two._1)
		}


	def leaveLocation(timestamp: Int, name: String, isEmployee: Boolean, roomId: Int) =
		{
		var status = 255
		val person = people find { x => x.name == name && x.isEmployee == isEmployee }

		if (person != None)
			{
			if (roomId == person.get.location)
				{
				val room = rooms.find(_.id == roomId)

				if (room != None)
					room.get.population -= 1

				person.get.location = if (roomId >= 0) -1
						      else -2

				val action = person.get.actions.find(_.lTime == person.get.location)
				action.get.lTime = timestamp
				this.timestamp = timestamp
				status = 0
				}
			}

		status
		}


	def enterLocation(timestamp: Int, name: String, isEmployee: Boolean, roomId: Int) =
		{
		var status = 255
		var person = people find { x => x.name == name && x.isEmployee == isEmployee }

		/* PERSON IS ENTERING A ROOM */
		if (roomId >= 0 && person != None)
			{
			if (person.get.location == -1)
				{
				val room = rooms find { x => x.id == roomId }

				if (room != None)
					room.get.population += 1
				else
				    rooms += new Room(roomId)

				person.get.location = roomId
				this.timestamp = timestamp

				person.get.actions += new Action(roomId, timestamp, -1)

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
				if (person.get.location == -2)
					{
					person.get.location = -1
					this.timestamp = timestamp
					person.get.actions += new Action(roomId, timestamp, -2)

					status = 0
					}
				}
			}

		status
		}
	}
