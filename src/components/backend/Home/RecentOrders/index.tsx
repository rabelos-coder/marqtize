'use client'

import { useTranslations } from 'next-intl'
import { Card, CardHeader, Col } from 'reactstrap'

import DropdownCommon from '@/components/common/DropdownCommon'

import RecentChart from './RecentChart'

const RecentOrders = () => {
  const t = useTranslations()

  return (
    <Col xxl={4} xl={7} md={6} sm={5} className="box-col-6">
      <Card className="height-equal">
        <CardHeader className="card-no-border">
          <div className="header-top">
            <h5>{t('recentOrders')}</h5>
            <div className="card-header-right-icon">
              <DropdownCommon
                dropdownMain={{
                  className: 'icon-dropdown',
                  direction: 'start',
                }}
                options={['Weekly', 'Monthly', 'Yearly']}
                icon
                iconName="icon-more-alt"
                btn={{ tag: 'span' }}
              />
            </div>
          </div>
        </CardHeader>
        <RecentChart />
      </Card>
    </Col>
  )
}

export default RecentOrders
