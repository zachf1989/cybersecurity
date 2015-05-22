package capstone.person

import scala.collection.mutable.ArrayBuffer
import capstone.action.Action


class Person (val name: String, val isEmployee: Boolean, var location: Int,
              val actions: ArrayBuffer[Action]) extends Serializable
  	{
	def this (name: String, isEmployee: Boolean)
		{
		this (name, isEmployee, -2, new ArrayBuffer[Action])
		}
	}