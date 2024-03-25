'use client'

import { useTranslations } from 'next-intl'
import { Card, CardHeader, Col } from 'reactstrap'

import DropdownCommon from '@/components/common/DropdownCommon'

import ActivityCardBody from './ActivityCardBody'

const ActivityCard = () => {
  const t = useTranslations()

  return (
    <Col xxl={4} xl={5} md={6} sm={7} className="notification box-col-6">
      <Card className="height-equal">
        <CardHeader className="card-no-border">
          <div className="header-top">
            <h5 className="m-0">{t('activity')}</h5>
            <div className="card-header-right-icon">
              <DropdownCommon
                icon={false}
                options={['Today', 'Tomorrow', 'Yesterday']}
                btn={{ caret: true, color: 'Outline-primary' }}
              />
            </div>
          </div>
        </CardHeader>
        <ActivityCardBody />
      </Card>
    </Col>
  )
}

export default ActivityCard
