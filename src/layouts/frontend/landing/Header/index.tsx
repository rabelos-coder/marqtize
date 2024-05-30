'use client'

import dynamic from 'next/dynamic'
import { Col, Container, Row } from 'reactstrap'

const SvgBorder = dynamic(
  () => import('@/components/frontend/common/SvgBorder'),
  { ssr: false }
)

type HeaderProps = {
  title?: string | React.ReactNode
  description?: string | React.ReactNode
  children?: React.ReactNode
  className?: string
  style?: React.CSSProperties
}

const Header = ({
  title,
  description,
  className,
  children,
  style,
}: HeaderProps) => {
  return (
    <header
      className="page-header-ui page-header-ui-dark bg-gradient-primary-to-secondary"
      style={{ ...style }}
    >
      <div
        className={`page-header-ui-content ${className ? className : 'pt-10'}`}
      >
        {title ? (
          <Container className="px-5 text-center">
            <Row className="gx-5 justify-content-center">
              <Col lg={8}>
                <h1 className="page-header-ui-title mb-3">{title}</h1>
                {description && (
                  <p className="page-header-ui-text">{description}</p>
                )}
              </Col>
            </Row>
          </Container>
        ) : (
          children
        )}
      </div>
      <SvgBorder className="text-white" />
    </header>
  )
}

export default Header
