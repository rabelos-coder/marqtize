'use client'

import { DateTime } from 'luxon'
import { useLocale, useTranslations } from 'next-intl'
import { ExpanderComponentProps } from 'react-data-table-component'

import { APP_DATETIME_FORMAT } from '@/environment'
import { useAuth } from '@/hooks'
import { Logging } from '@/types/logging'

export const ExpandedComponent: React.FC<ExpanderComponentProps<Logging>> = ({
  data,
}) => {
  const t = useTranslations()

  const locale = useLocale()
  const { timezone } = useAuth()

  return (
    <div className="p-3">
      <table cellPadding={5} cellSpacing={0} border={0}>
        <tbody>
          <tr>
            <th className="font-weight-bold">{t('id')}:</th>
            <td>{data.id}</td>
          </tr>
          <tr>
            <th className="font-weight-bold">{t('createdAt')}:</th>
            <td>
              {DateTime.fromISO(data.createdAt)
                .setZone(timezone)
                .setLocale(locale)
                .toFormat(APP_DATETIME_FORMAT)}
            </td>
          </tr>
          <tr>
            <th className="font-weight-bold">{t('updatedAt')}:</th>
            <td>
              {DateTime.fromISO(data.updatedAt)
                .setZone(timezone)
                .setLocale(locale)
                .toFormat(APP_DATETIME_FORMAT)}
            </td>
          </tr>
        </tbody>
      </table>
    </div>
  )
}
