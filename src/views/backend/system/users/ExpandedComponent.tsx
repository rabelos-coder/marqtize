'use client'

import { DateTime } from 'luxon'
import { useLocale, useTranslations } from 'next-intl'
import { ExpanderComponentProps } from 'react-data-table-component'

import { APP_DATETIME_FORMAT } from '@/environment'
import { useAuth } from '@/hooks'
import { User } from '@/types/user'

export const ExpandedComponent: React.FC<ExpanderComponentProps<User>> = ({
  data,
}) => {
  const t = useTranslations()

  const locale = useLocale()
  const { timezone, user } = useAuth()

  return (
    <div className="p-3">
      <table cellPadding={5} cellSpacing={0} border={0}>
        <tbody>
          <tr>
            <th className="font-weight-bold">{t('id')}:</th>
            <td>{data.id}</td>
          </tr>
          {user?.isSuperAdmin && (
            <tr>
              <th className="font-weight-bold">{t('superAdmin')}:</th>
              <td>
                <span
                  className={`badge badge-light-${data.isActive ? 'success' : 'danger'}`}
                >
                  {data.isActive ? t('yes') : t('no')}
                </span>
              </td>
            </tr>
          )}
          <tr>
            <th className="font-weight-bold">{t('language')}:</th>
            <td>{t(data.language)}</td>
          </tr>
          <tr>
            <th className="font-weight-bold">{t('timezone')}:</th>
            <td>{data.timezone?.name}</td>
          </tr>
          <tr>
            <th className="font-weight-bold">
              {t('rolePlural', { count: data.roles?.length ?? 0 })}:
            </th>
            <td>
              {data.roles?.length
                ? data.roles?.map((role) => role.name)?.join(', ')
                : t('none', { gender: 'female' })}
            </td>
          </tr>
          <tr>
            <th className="font-weight-bold">{t('customer')}:</th>
            <td>
              {data.customer?.systemName ??
                data.customer?.tradingName ??
                data.customer?.corporateName ??
                t('none', { gender: 'male' })}
            </td>
          </tr>
          <tr>
            <th className="font-weight-bold">{t('accounts')}:</th>
            <td>
              {data.accounts?.length
                ? data.accounts?.map((a) => t(a.provider)).join(', ')
                : t('none', { gender: 'female' })}
            </td>
          </tr>
          <tr>
            <th className="font-weight-bold">{t('sessions')}:</th>
            <td>
              {data.sessions?.length
                ? data.sessions.filter(
                    ({ expiresAt }) =>
                      DateTime.fromISO(expiresAt)
                        .setZone(timezone)
                        .setLocale(locale)
                        .toJSDate() >=
                      DateTime.now()
                        .setZone(timezone)
                        .setLocale(locale)
                        .toJSDate()
                  ).length
                : t('none', { gender: 'female' })}
            </td>
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
          {data.deletedAt && (
            <tr>
              <th className="font-weight-bold">{t('deletedAt')}:</th>
              <td>
                {DateTime.fromISO(data.deletedAt)
                  .setZone(timezone)
                  .setLocale(locale)
                  .toFormat(APP_DATETIME_FORMAT)}
              </td>
            </tr>
          )}
        </tbody>
      </table>
    </div>
  )
}
