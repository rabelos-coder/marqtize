'use client'

import { useTranslations } from 'next-intl'
import { Card, CardHeader } from 'reactstrap'

import DropdownCommon from '@/components/common/DropdownCommon'
import { WeeklyMonDropdown } from '@/fake'

import FollowerChart from './FollowerChart'

const FollowerGrowth = () => {
  const t = useTranslations()

  return (
    <Card className="growth-wrap">
      <CardHeader className="card-no-border">
        <div className="header-top">
          <h5>{t('followersGrowth')}</h5>
          <DropdownCommon
            dropdownMain={{ className: 'icon-dropdown', direction: 'start' }}
            options={WeeklyMonDropdown}
            icon
            iconName="icon-more-alt"
            btn={{ tag: 'span', color: 'Outline-primary' }}
          />
        </div>
      </CardHeader>
      <FollowerChart />
    </Card>
  )
}

export default FollowerGrowth
