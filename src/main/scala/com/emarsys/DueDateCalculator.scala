package com.emarsys

import org.joda.time.{Duration, DateTime, Interval}

/**
  * Created by andra on 2016. 02. 09..
  */
object DueDateCalculator {
  val workDay = Duration.standardHours(8)

  def reduce(weeks: Int, days: Int, excessDuration: Duration)(remainingWorkDays: Int, remainingTimeOfDay: Duration): (Int, Int, Duration) = (weeks, days, excessDuration) match {
    case (weeks, days, excessDuration) if days > remainingWorkDays =>
      reduce(weeks+1, days-5, excessDuration)(remainingWorkDays, remainingTimeOfDay)

    case (weeks, days, excessDuration) if excessDuration.isShorterThan(remainingTimeOfDay) =>
      (weeks, days, excessDuration)

    case _ =>
      reduce(weeks, days + 1, excessDuration.minus(workDay))(remainingWorkDays, remainingTimeOfDay)
  }


  def calculateDueDate(reportDate: DateTime, turnAround: Duration): DateTime = reportDate match {
    case d if d.getDayOfWeek() >= 6 =>
      throw new IllegalArgumentException("reportDate should be on weekdays between 9am to 5pm.")

    case d if d.getHourOfDay < 9 || d.getHourOfDay >= 17 =>
      throw new IllegalArgumentException("reportDate should be on weekdays between 9am to 5pm.")

    case validReportDate => {
      val endOfWorkDay = validReportDate.withHourOfDay(17).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(0)
      val remainingWorkdays = 5-validReportDate.getDayOfWeek
      val (extraWeeks, extraDays, excessDuration) = reduce(0, 0, turnAround)(remainingWorkdays, new Duration(validReportDate, endOfWorkDay))
      validReportDate.plusWeeks(extraWeeks).plusDays(extraDays).plus(excessDuration)
    }

  }
}
