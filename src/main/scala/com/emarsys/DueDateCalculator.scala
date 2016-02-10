package com.emarsys

import org.joda.time.{DateTime, Duration}
import com.emarsys.utils.DueDateUtils._

/**
  * Created by andra on 2016. 02. 09..
  */
object DueDateCalculator {

  /**
    * First solution. method is called recursively, and reduce the turnaround time until we can add it to current time before end of workday
    *
    * @param currentDate
    * @param remainingTurnAround
    * @return
    */
  def getToDueDAte(currentDate: DateTime, remainingTurnAround: Duration): DateTime = (currentDate, remainingWorkhours(currentDate)) match {
    case (cd, rw) if remainingTurnAround.isShorterThan(rw) =>
      currentDate.plus(remainingTurnAround)
    case (cd, rw) =>
      getToDueDAte(currentDate.plus(shift(cd)).plus(rw), remainingTurnAround.minus(rw))
  }

  /**
    * Second solution using a stream, the stream generates hourly the moments initiated with reporting date as head
    *
    * @param start
    * @return
    */
  def workHours(start: DateTime): Stream[DateTime] = start #:: {
    (start, remainingWorkhours(start)) match {
      case (d, rw) if rw.isLongerThan(Duration.standardHours(1)) =>
        workHours(d.plusHours(1))
      case _ =>
        workHours(start.plus(shift(start)).plusHours(1))
    }
  }


  /**
    * The requested method shoudl be implemented by solving the coding challenge of Emarsys
    * @param reportDate The exact date when the task has been reported i a reporting system
    * @param turnAround The turnaround time is given in working hours
    * @return
    */
  def calculateDueDate(reportDate: DateTime, turnAround: Duration): DateTime = reportDate match {
    case d if d.getDayOfWeek() > 5 =>
      throw new IllegalArgumentException("reportDate should be on weekdays between 9am to 5pm.")

    case d if d.getHourOfDay < 9 || d.getHourOfDay >= 17 =>
      throw new IllegalArgumentException("reportDate should be on weekdays between 9am to 5pm.")

    case validReportDate => {
      /**
        * Frist solution
        */
      //getToDueDAte(reportDate, turnAround)

      /**
        * Second solution - we initiate the of the stream with the reportDate and get the nth element of the stream (n = turnAroundduration in hours)
        * I stick to use the turnaround time as a duration to make room for further modifications
        */
      workHours(reportDate)(turnAround.getStandardHours.toInt)
    }

  }
}
