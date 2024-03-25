'use client'

import { useTranslations } from 'next-intl'
import { Card, CardHeader } from 'reactstrap'

import DropdownCommon from '@/components/common/DropdownCommon'
import { WeeklyMonDropdown } from '@/fake'

import TotalUserBody from './TotalUserBody'

const TotalUser = () => {
  const t = useTranslations()

  return (
    <Card>
      <CardHeader className="card-no-border">
        <div className="header-top">
          <h5>{t('totalUsers')}</h5>
          <DropdownCommon
            dropdownMain={{ className: 'icon-dropdown', direction: 'start' }}
            options={WeeklyMonDropdown}
            icon
            iconName="icon-more-alt"
            btn={{ tag: 'span' }}
          />
        </div>
      </CardHeader>
      <TotalUserBody />
    </Card>
  )
}

export default TotalUser
