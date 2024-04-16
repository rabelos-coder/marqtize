import { getTranslations } from 'next-intl/server'
import { Col, Container, List, Row } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import { APP_META_TITLE, EMAIL } from '@/environment'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('privacyPolicy'))

  return {
    title,
  }
}

export default async function PrivacyPolicyPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: 'privacyPolicy' })

  return (
    <LandingLayout>
      <Header title={t('title')} description={t('description')} />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="mb-5">
            <Col>
              <p className="mb-3 text-justify">
                <strong>1.</strong>{' '}
                {t('paragraph1A', { company: APP_META_TITLE })}
              </p>
              <p className="mb-3 text-justify">{t('paragraph1B')}</p>
              <p className="mb-3 text-justify">{t('paragraph1C')}</p>
              <h4 className="my-4">{t('subtitle1')}</h4>
              <p className="mb-3 text-justify">
                <strong>2.</strong> {t('paragraph2')}
              </p>
              <List className="mb-3 text-justify">
                <li>{t('paragraph2Dot1')}</li>
                <li>{t('paragraph2Dot2')}</li>
                <li>{t('paragraph2Dot3', { company: APP_META_TITLE })}</li>
                <li>{t('paragraph2Dot4')}</li>
                <li>{t('paragraph2Dot5')}</li>
                <li>{t('paragraph2Dot6')}</li>
                <li>{t('paragraph2Dot7', { company: APP_META_TITLE })}</li>
              </List>
              <p className="mb-3 text-justify">
                <strong>a.</strong> {t('paragraph2DotA')}
              </p>
              <p className="mb-3 text-justify">
                <strong>3.</strong> {t('paragraph3')}
              </p>
              <p className="mb-3 text-justify">
                <strong>4.</strong>{' '}
                {t.rich('paragraph4', {
                  b: (chunk) => <strong>{chunk}</strong>,
                })}
              </p>
              <h4 className="my-4">{t('subtitle2')}</h4>
              <p className="mb-3 text-justify">
                <strong>5.</strong>{' '}
                {t('paragraph5', { company: APP_META_TITLE })}
              </p>
              <h4 className="my-4">{t('subtitle3')}</h4>
              <p className="mb-3 text-justify">
                <strong>6.</strong>{' '}
                {t('paragraph6', { company: APP_META_TITLE })}
              </p>
              <p className="mb-3 text-justify">
                <strong>7.</strong> {t('paragraph7', { email: EMAIL })}
              </p>
              <p className="mb-3 text-justify">
                <strong>8.</strong>{' '}
                {t('paragraph8', { company: APP_META_TITLE })}
              </p>
              <p className="mb-3 text-justify">
                <strong>9.</strong> {t('paragraph9')}
              </p>
              <p className="mb-3 text-justify">
                <strong>10.</strong>{' '}
                {t('paragraph10', { company: APP_META_TITLE })}
              </p>
              <h4 className="my-4">{t('subtitle3')}</h4>
              <p className="mb-3 text-justify">
                <strong>11.</strong> {t('paragraph11')}
              </p>
              <p className="mb-3 text-justify">
                <strong>12.</strong> {t('paragraph12')}
              </p>
              <p className="mb-3 text-justify">
                <strong>13.</strong>{' '}
                {t('paragraph13', { company: APP_META_TITLE })}
              </p>
              <h4 className="my-4">{t('subtitle4')}</h4>
              <p className="mb-3 text-justify">
                <strong>14.</strong> {t('paragraph14')}
              </p>
              <p className="mb-3 text-justify">
                <strong>15.</strong>{' '}
                {t('paragraph15', { company: APP_META_TITLE })}
              </p>
              <p className="mb-3 text-justify">
                <strong>16.</strong> {t('paragraph16')}
              </p>
              <p className="mb-3 text-justify">
                <strong>17.</strong> {t('paragraph17', { email: EMAIL })}
              </p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
