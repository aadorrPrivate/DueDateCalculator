package com.emarsys

import com.emarsys.utils.DueDateUtils
import org.joda.time.{Duration, DateTime, Interval}

/**
  * Created by andra on 2016. 02. 09..
  */
object DueDateCalculator {

  def getToDueDAte(currentDate: DateTime, remainingTurnAround: Duration): DateTime = (currentDate, DueDateUtils.remainingWorkhours(currentDate)) match {
    case (cd, rw) if remainingTurnAround.isShorterThan(rw) =>
      currentDate.plus(remainingTurnAround)
    case (cd, rw) =>
      getToDueDAte(currentDate.plus(DueDateUtils.shift(cd)).plus(rw), remainingTurnAround.minus(rw))
  }

  def calculateDueDate(reportDate: DateTime, turnAround: Duration): DateTime = reportDate match {
    case d if d.getDayOfWeek() > 5 =>
      throw new IllegalArgumentException("reportDate should be on weekdays between 9am to 5pm.")

    case d if d.getHourOfDay < 9 || d.getHourOfDay >= 17 =>
      throw new IllegalArgumentException("reportDate should be on weekdays between 9am to 5pm.")

    case validReportDate => {
      getToDueDAte(reportDate, turnAround)
    }

  }
}
