import dynamic from 'next/dynamic'
import { Row } from 'reactstrap'

import { useLayout } from '@/hooks'

const LeftBar = dynamic(() => import('./Leftbar'), { ssr: false })
const RightBar = dynamic(() => import('./Rightbar'), { ssr: false })

const Header = () => {
  const { sideBarToggle } = useLayout()

  return (
    <div className={`page-header ${sideBarToggle ? 'close_icon' : ''}`}>
      <Row className="header-wrapper m-0">
        <LeftBar />
        <RightBar />
      </Row>
    </div>
  )
}

export default Header
