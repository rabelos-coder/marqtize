import dynamic from 'next/dynamic'
import { Col } from 'reactstrap'

import { THEME_LOCALE_SWITCHER_ENABLED } from '@/environment'

const Language = dynamic(() => import('./Languages'), { ssr: false })
const MoonLight = dynamic(() => import('./MoonLight'), { ssr: false })
const Profile = dynamic(() => import('./Profile'), { ssr: false })

const Rightbar = () => {
  return (
    <Col
      xxl={7}
      xl={6}
      md={7}
      xs={8}
      className="nav-right pull-right right-header p-0 ms-auto"
    >
      <ul className="nav-menus flex-row">
        {THEME_LOCALE_SWITCHER_ENABLED && <Language />}
        {/* <SearchBar /> */}
        <MoonLight />
        <Profile />
      </ul>
    </Col>
  )
}

export default Rightbar
