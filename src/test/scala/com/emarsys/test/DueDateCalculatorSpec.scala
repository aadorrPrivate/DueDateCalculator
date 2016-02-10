package com.emarsys.test

import com.emarsys.DueDateCalculator
import com.sun.javaws.exceptions.InvalidArgumentException
import org.joda.time.{Duration, Interval, DateTime}
import org.scalatest._

class DueDateCalculatorSpec extends FlatSpec with Matchers {

  "A DueDateCalculator" should "return the same time if turnaround time is 0" in {
    val reportingDate = new DateTime(2016, 2, 9, 12, 0, 0, 0)
    DueDateCalculator.calculateDueDate(reportingDate, Duration.ZERO) should be (reportingDate)
  }

  it should "return a valid due date for turnaround time longer than 8 hours" in {
    val reportingDate = new DateTime(2016, 2, 9, 10, 0, 0, 0)
    val dueDate = new DateTime(2016, 2, 10, 11, 0, 0, 0)
    DueDateCalculator.calculateDueDate(reportingDate, Duration.standardHours(9)) should be (dueDate)
  }

  it should "return a valid due date for turnaround time longer than on week" in {
    val reportingDate = new DateTime(2016, 2, 9, 16, 0, 0, 0)
    val dueDate = new DateTime(2016, 2, 17, 10, 0, 0, 0)
    DueDateCalculator.calculateDueDate(reportingDate, Duration.standardHours(5*8+2)) should be (dueDate)
  }

  it should "return a valid due date for turnaround time longer than several weeks" in {
    val reportingDate = new DateTime(2016, 2, 9, 16, 0, 0, 0)
    val dueDate = new DateTime(2016, 3, 7, 11, 0, 0, 0)
    DueDateCalculator.calculateDueDate(reportingDate, Duration.standardHours(18*8+3)) should be (dueDate)
  }


  it should "return a valid due date for turnaround time longer than remaining working hours but less than 8 hours" in {
    val reportingDate = new DateTime(2016, 2, 9, 16, 0, 0, 0)
    val dueDate = new DateTime(2016, 2, 10, 11, 0, 0, 0)
    DueDateCalculator.calculateDueDate(reportingDate, Duration.standardHours(3)) should be (dueDate)
  }

  it should "return a valid due date  to 10 am for reporting time 9 am, turnaround time 1 hour (edge conditions)" in {
    val reportingDate = new DateTime(2016, 2, 9, 9, 0, 0, 0)
    val dueDate = new DateTime(2016, 2, 9, 10, 0, 0, 0)
    DueDateCalculator.calculateDueDate(reportingDate, Duration.standardHours(1)) should be (dueDate)
  }

  it should "throw InvalidArgumentException if the provided date is on a workday but out of working hours (edge conditions)" in {
    val reportingDate = new DateTime(2016, 2, 9, 17, 0, 0, 0)
    a [IllegalArgumentException] should be thrownBy {
      DueDateCalculator.calculateDueDate(reportingDate, Duration.standardHours(1))
    }
  }

  it should "throw InvalidArgumentException if the provided date is on a workday but out of working hours" in {
    val reportingDate = new DateTime(2016, 2, 9, 18, 0, 0, 0)
    a [IllegalArgumentException] should be thrownBy {
      DueDateCalculator.calculateDueDate(reportingDate, Duration.standardHours(1))
    }
  }

  it should "throw InvalidArgumentException if the provided date is in the weekend" in {
    val reportingDate = new DateTime(2016, 2, 7, 15, 0, 0, 0)
    a [IllegalArgumentException] should be thrownBy {
      DueDateCalculator.calculateDueDate(reportingDate, Duration.standardHours(1))
    }
  }
}