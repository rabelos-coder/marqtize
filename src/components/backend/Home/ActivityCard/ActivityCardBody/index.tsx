import { CardBody } from 'reactstrap'

import { ActivityData } from '@/fake'

const ActivityCardBody = () => {
  return (
    <CardBody className="pt-0">
      <ul>
        {ActivityData.map((item, i) => (
          <li key={i} className="d-flex">
            <div className={`activity-dot-${item.color}`} />
            <div className="w-100 ms-3">
              <p className="d-flex justify-content-between mb-2">
                <span className="date-content light-background">
                  {item.subTitle}
                </span>
                <span>{item.time}</span>
              </p>
              <h6>
                {item.title}
                <span className="dot-notification" />
              </h6>
              <p className="f-light">{item.dis}</p>
            </div>
          </li>
        ))}
      </ul>
    </CardBody>
  )
}

export default ActivityCardBody
