package com.emarsys

import com.emarsys.utils.DueDateUtils.*

import java.time.{Duration, LocalDateTime}
import scala.annotation.tailrec

/**
 * Emarsys coding challenge - homework
 * DueDateCalculator object containing methods to calculate due dates based on reporting date and turnaround time.
 */
object DueDateCalculator {

  /**
   * First solution. method is called recursively, and reduce the turnaround time until we can add it to current time before end of workday
   *
   * @param currentDate The current date during the calculation
   * @param remainingTurnAround The remaining turnaround time in hours
   * @return
   */
  @tailrec
  private def getToDueDAte(currentDate: LocalDateTime, remainingTurnAround: Duration): LocalDateTime = (currentDate, remainingWorkhours(currentDate)) match {
    case (cd, rw) if remainingTurnAround.compareTo(rw) < 0 =>
      currentDate.plus(remainingTurnAround)

    case (cd, rw) =>
      getToDueDAte(currentDate.plus(shift(cd)).plus(rw), remainingTurnAround.minus(rw))
  }

  /**
   * Second solution using a stream, the stream generates hourly the moments initiated with reporting date as head
   *
   * @param start The current date during calulation while generating the stream
   * @return
   */
  private def workHours(start: LocalDateTime): LazyList[LocalDateTime] = start #:: {
    (start, remainingWorkhours(start)) match {
      case (d, rw) if rw.compareTo(Duration.ofHours(1)) > 0 =>
        workHours(d.plusHours(1))
      case _ =>
        workHours(start.plus(shift(start)).plusHours(1))
    }
  }

  /**
   * Calculate the due date based on the reporting date and the turnaround time
   *
   * @param reportDate The exact date when the task has been reported in a reporting system
   * @param turnAround The turnaround time is given in working hours
   * @return
   */
  def calculateDueDate(reportDate: LocalDateTime, turnAround: Duration): LocalDateTime = reportDate match {
    case d if d.getDayOfWeek.getValue > 5 =>
      throw new IllegalArgumentException("reportDate should be on weekdays between 9am to 5pm.")

    case d if d.getHour < 9 || d.getHour >= 17 =>
      throw new IllegalArgumentException("reportDate should be on weekdays between 9am to 5pm.")

    case validReportDate =>

      /**
       * First solution
       */
      //getToDueDAte(reportDate, turnAround)

      /**
       * Second solution - we initiate the of the stream with the reportDate and get the nth element of the stream (n = turnAroundduration in hours)
       * I stick to use the turnaround time as a duration to make room for further modifications
       */
      workHours(reportDate)(turnAround.toHours.toInt)

  }
}
