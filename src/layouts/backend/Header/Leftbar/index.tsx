import dynamic from 'next/dynamic'
import Image from 'next/image'
import { FiAlignCenter } from 'react-icons/fi'
import { Col } from 'reactstrap'

import { APP_META_TITLE } from '@/environment'
import { useLayout } from '@/hooks'
import { Link } from '@/navigation'

const Trading = dynamic(() => import('./Trading'))

const Leftbar = () => {
  const { sideBarToggle, setSideBarToggle } = useLayout()

  return (
    <>
      <Col className="header-logo-wrapper col-auto p-0">
        <div className="logo-wrapper">
          <Link href={'/backend'}>
            <Image
              className="img-fluid for-light"
              src={`/assets/images/logo/marqtize_logo.png`}
              alt={APP_META_TITLE}
              width={100}
              height={100}
            />
            <Image
              className="img-fluid for-dark"
              src={`/assets/images/logo/marqtize_logo_dark.png`}
              alt={APP_META_TITLE}
              width={100}
              height={100}
            />
          </Link>
        </div>
        <div
          className="toggle-sidebar"
          onClick={() => setSideBarToggle(!sideBarToggle)}
        >
          <FiAlignCenter
            className="status_toggle middle sidebar-toggle"
            id="sidebar-toggle"
          />
        </div>
      </Col>
      <Col xxl={5} xl={6} lg={5} md={4} sm={3} className="left-header p-0">
        <Trading />
      </Col>
    </>
  )
}

export default Leftbar
