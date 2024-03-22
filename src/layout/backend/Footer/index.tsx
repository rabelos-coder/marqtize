import { useTranslations } from 'next-intl'
import React from 'react'
import { Col, Container, Row } from 'reactstrap'

import { APP_META_TITLE } from '@/environment'

export const Footer = () => {
  const t = useTranslations()

  return (
    <footer className="footer">
      <Container fluid={true}>
        <Row>
          <Col md={12} className="footer-copyright text-center">
            <p className="mb-0">
              {t('copyright', {
                year: new Date().getFullYear(),
                company: APP_META_TITLE,
              })}
            </p>
          </Col>
        </Row>
      </Container>
    </footer>
  )
}
