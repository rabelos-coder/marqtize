import { useTranslations } from 'next-intl'
import React from 'react'
import { Col, Container, Row } from 'reactstrap'

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
                company: "Rabelo's Coder",
              })}
            </p>
          </Col>
        </Row>
      </Container>
    </footer>
  )
}
