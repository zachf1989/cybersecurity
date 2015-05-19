package capstone.person

import scala.collection.mutable.ArrayBuffer


class Person(val name: String, val isEmployee: Boolean, var location: Int, val actions: ArrayBuffer[(Int, Int)]) extends Serializable
  	{
	def this(name: String, isEmployee: Boolean)
		{
		this(name, isEmployee, -2, new ArrayBuffer[(Int, Int)])
		}
	}
