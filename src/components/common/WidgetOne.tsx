'use client'

import { Card, CardBody } from 'reactstrap'

import SvgIcon from './SvgIcon'

type WidgetProps = {
  data: {
    title: string
    gross: number
    total: number | string
    color: string
    icon: string
  }
}

const WidgetOne = ({ data }: WidgetProps) => {
  return (
    <Card className="widget-1">
      <CardBody>
        <div className="widget-content">
          <div className={`widget-round ${data.color}`}>
            <div className="bg-round">
              <SvgIcon className="svg-fill" iconId={`${data.icon}`} />
              <SvgIcon className="half-circle svg-fill" iconId="halfcircle" />
            </div>
          </div>
          <div>
            <h4>{data.total}</h4>
            <span className="f-light">{data.title}</span>
          </div>
        </div>
        <div className={`font-${data.color} f-w-500`}>
          <i
            className={`icon-arrow-${data.gross < 50 ? 'down' : 'up'} icon-rotate me-1`}
          />
          <span>{`${data.gross < 50 ? '-' : '+'}${data.gross}%`}</span>
        </div>
      </CardBody>
    </Card>
  )
}

export default WidgetOne
