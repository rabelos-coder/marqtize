import { CardBody } from 'reactstrap'

import UserMinus from './UserMinus'
import UserPlus from './UserPlus'

const TotalUserBody = () => {
  return (
    <CardBody className="pt-0">
      <ul className="user-list">
        <UserPlus />
        <UserMinus />
      </ul>
    </CardBody>
  )
}

export default TotalUserBody
