import 'moment/locale/pt-br'

import moment from 'moment-timezone'

import { APP_DATETIME_FORMAT, APP_LANGUAGE, APP_TIMEZONE } from '@/environment'

export class DateTime {
  date
  dateTime
  dateFormat = APP_DATETIME_FORMAT
  dateLocale = APP_LANGUAGE
  dateTimezone = APP_TIMEZONE

  constructor(dateTime: moment.MomentInput) {
    this.dateTime = dateTime
    this.date = moment(this.dateTime)
  }

  locale(locale: string) {
    this.dateLocale = locale

    return this
  }

  timeZone(timezone: string) {
    this.dateTimezone = timezone

    return this
  }

  startOf(unitOfTime: moment.unitOfTime.StartOf) {
    this.date.startOf(unitOfTime)

    return this
  }

  endOf(unitOfTime: moment.unitOfTime.StartOf) {
    this.date.endOf(unitOfTime)

    return this
  }

  diff(
    b: moment.MomentInput,
    unitOfTime?: moment.unitOfTime.Diff | undefined,
    precise?: boolean | undefined
  ) {
    return this.date.diff(b, unitOfTime, precise)
  }

  subtract(
    amount?: moment.DurationInputArg1,
    unit?: moment.unitOfTime.DurationConstructor | undefined
  ) {
    this.date.subtract(amount, unit)

    return this
  }

  add(
    amount?: moment.DurationInputArg1,
    unit?: moment.unitOfTime.DurationConstructor | undefined
  ) {
    this.date.add(amount, unit)

    return this
  }

  format(format?: string | undefined) {
    if (format) this.dateFormat = format

    return this.date
      .tz(this.dateTimezone)
      .locale(this.dateLocale)
      .format(this.dateFormat)
  }

  calendar() {
    return this.date.tz(this.dateTimezone).locale(this.dateLocale).calendar()
  }
}
