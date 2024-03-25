'use client'

import { DateTime } from 'luxon'
import { useLocale, useTranslations } from 'next-intl'
import { ExpanderComponentProps } from 'react-data-table-component'

import { APP_DATETIME_FORMAT } from '@/environment'
import { useAuth } from '@/hooks'
import { Role } from '@/types/role'

export const ExpandedComponent: React.FC<ExpanderComponentProps<Role>> = ({
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

          <tr>
            <th className="font-weight-bold">{t('users')}:</th>
            <td>{data.users?.length ?? t('none', { gender: 'male' })}</td>
          </tr>

          <tr>
            <th className="font-weight-bold">{t('permissions')}:</th>
            <td>
              {data.claims.length
                ? data.claims?.map((c) => c).join(', ')
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
          {user?.isSuperAdmin && (
            <tr>
              <th className="font-weight-bold">{t('isDeleteable')}:</th>
              <td>
                <span
                  className={`badge badge-light-${data.isDeleteable ? 'success' : 'danger'}`}
                >
                  {data.isDeleteable ? t('yes') : t('no')}
                </span>
              </td>
            </tr>
          )}
          <tr>
            <th className="font-weight-bold">{t('isDefault')}:</th>
            <td>
              <span
                className={`badge badge-light-${data.isDefault ? 'success' : 'danger'}`}
              >
                {data.isDefault ? t('yes') : t('no')}
              </span>
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
