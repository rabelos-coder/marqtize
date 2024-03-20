import { Row } from 'reactstrap'

import { useLayout } from '@/hooks'

import { Leftbar } from './Leftbar'
import { Rightbar } from './Rightbar'

export const Header = () => {
  const { sideBarToggle } = useLayout()

  return (
    <div className={`page-header ${sideBarToggle ? 'close_icon' : ''}`}>
      <Row className="header-wrapper m-0">
        {/* <Search /> */}
        <Leftbar />
        <Rightbar />
      </Row>
    </div>
  )
}
