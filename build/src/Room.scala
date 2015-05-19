package capstone.room


class Room(val id: Int, var population: Int) extends Serializable
    {
    def this(id: Int)
        {
        this(id, 1)
        }
    }