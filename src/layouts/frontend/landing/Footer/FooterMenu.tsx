'use client'

import Image from 'next/image'
import { useTranslations } from 'next-intl'
import { useState } from 'react'
import { FaFacebook } from '@react-icons/all-files/fa/FaFacebook'
import { FaInstagram } from '@react-icons/all-files/fa/FaInstagram'
import { Col, List, Row } from 'reactstrap'

import { APP_META_TITLE, FACEBOOK_URL, INSTAGRAM_URL } from '@/environment'
import { Link, usePathname } from '@/navigation'

const FooterMenu = () => {
  const t = useTranslations()

  const pathname = usePathname()
  const [imgSrc] = useState(
    pathname === '/'
      ? '/assets/images/logo/marqtize_logo.png'
      : '/assets/images/logo/marqtize_logo_dark.png'
  )

  return (
    <>
      <Row className="gx-5">
        <Col lg={3}>
          <div className="footer-brand">
            <Image
              src={imgSrc}
              width={180}
              height={180}
              alt={APP_META_TITLE}
              className="img-fluid me-2 mb-3"
            />
          </div>
          <div className="mb-3">
            {t('madeWithLove', { company: APP_META_TITLE })}
          </div>
          <div className="icon-list-social mb-5">
            <Link
              className="icon-list-social-link"
              href={INSTAGRAM_URL}
              target="_blank"
            >
              <FaInstagram />
            </Link>
            <Link
              className="icon-list-social-link"
              href={FACEBOOK_URL}
              target="_blank"
            >
              <FaFacebook />
            </Link>
          </div>
        </Col>
        <Col lg={9}>
          <Row className="gx-5">
            <Col lg={4} md={6} className="mb-5 mb-lg-0">
              <div className="text-uppercase-expanded text-xs mb-4">
                {t('products')}
              </div>
              <List type="unstyled" className="mb-0">
                <li className="mb-2">
                  <Link href="/products/ecommerce-platform">
                    {t('productsMenu.ecommerce')}
                  </Link>
                </li>
                <li className="mb-2">
                  <Link href="/products/order-management-system">
                    {t('productsMenu.orderManagement')}
                  </Link>
                </li>
                <li className="mb-2">
                  <Link href="/products/design-management-system">
                    {t('productsMenu.designManagement')}
                  </Link>
                </li>
                <li className="mb-2">
                  <Link href="/products/marketplace-management-system">
                    {t('productsMenu.marketplaceManagement')}
                  </Link>
                </li>
                <li>
                  <Link href="/products/sellers-management-system">
                    {t('productsMenu.sellersManagement')}
                  </Link>
                </li>
              </List>
            </Col>
            <Col lg={4} md={6} className="mb-5 mb-lg-0">
              <div className="text-uppercase-expanded text-xs mb-4">
                {t('technical')}
              </div>
              <List type="unstyled" className="mb-0">
                <li className="mb-2">
                  <Link href="/documentation">{t('documentation.title')}</Link>
                </li>
                <li className="mb-2">
                  <Link href="/components">{t('components.title')}</Link>
                </li>
                <li className="mb-2">
                  <Link href="/changelog">{t('changelog.title')}</Link>
                </li>
              </List>
            </Col>
            <Col lg={4} md={6}>
              <div className="text-uppercase-expanded text-xs mb-4">
                {t('legal')}
              </div>
              <List type="unstyled" className="mb-0">
                <li className="mb-2">
                  <Link href="/privacy-policy">{t('privacyPolicy.title')}</Link>
                </li>
                <li className="mb-2">
                  <Link href="/terms-and-conditions">
                    {t('termsAndConditions.title')}
                  </Link>
                </li>
                <li>
                  <Link href="/license">{t('license.title')}</Link>
                </li>
              </List>
            </Col>
          </Row>
        </Col>
      </Row>
      <hr className="my-5"></hr>
    </>
  )
}

export default FooterMenu
