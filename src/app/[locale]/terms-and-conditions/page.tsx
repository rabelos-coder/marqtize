import { getTranslations } from 'next-intl/server'
import { Col, Container, List, Row } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import { APP_META_TITLE, EMAIL, WEBSITE } from '@/environment'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: 'termsAndConditions' })
  const title = concatTitle(t('title'))

  return {
    title,
  }
}

export default async function TermsAndConditionsPage({
  params: { locale },
}: any) {
  const t = await getTranslations({ locale, namespace: 'termsAndConditions' })

  return (
    <LandingLayout>
      <Header title={t('title')} description={t('description')} />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="mb-5">
            <Col>
              <p className="mb-3 text-justify">
                <strong>1.</strong>{' '}
                {t.rich('paragraph1', {
                  company: APP_META_TITLE,
                  b: (chunk) => <strong>{chunk}</strong>,
                })}
              </p>
              <h4 className="my-4">{t('subtitle1')}</h4>
              <p className="mb-3 text-justify">
                <strong>2.</strong>{' '}
                {t('paragraph2', {
                  company: APP_META_TITLE,
                  url: WEBSITE,
                })}
              </p>
              <p className="mb-3 text-justify">
                {t.rich('paragraph2A', {
                  b: (chunk) => <strong>{chunk}</strong>,
                })}
              </p>
              <p className="mb-3 text-justify">
                <strong>3.</strong>{' '}
                {t.rich('paragraph3', {
                  b: (chunk) => <strong>{chunk}</strong>,
                })}
              </p>
              <p className="mb-3 text-justify">
                <strong>4.</strong>{' '}
                {t('paragraph4', {
                  company: APP_META_TITLE,
                })}
              </p>
              <p className="mb-3 text-justify">
                <strong>5.</strong>{' '}
                {t.rich('paragraph5', {
                  company: APP_META_TITLE,
                  b: (chunk) => <strong>{chunk}</strong>,
                })}
              </p>
              <List className="mb-3 text-justify">
                <li>{t('paragraph5Dot1', { company: APP_META_TITLE })}</li>
                <li>{t('paragraph5Dot2', { company: APP_META_TITLE })}</li>
                <li>{t('paragraph5Dot3', { company: APP_META_TITLE })}</li>
                <li>{t('paragraph5Dot4')}</li>
                <li>{t('paragraph5Dot5')}</li>
                <li>{t('paragraph5Dot6')}</li>
                <li>{t('paragraph5Dot7')}</li>
              </List>
              <p className="mb-3 text-justify">
                <strong>6.</strong>{' '}
                {t('paragraph6', { company: APP_META_TITLE })}
              </p>
              <h4 className="my-4">{t('subtitle2')}</h4>
              <p className="mb-3 text-justify">
                <strong>7.</strong> {t('paragraph7')}
              </p>
              <p className="mb-3 text-justify">
                <strong>8.</strong> {t('paragraph8')}
              </p>
              <p className="mb-3 text-justify">
                <strong>9.</strong>{' '}
                {t.rich('paragraph9', {
                  b: (chunk) => <strong>{chunk}</strong>,
                })}
              </p>
              <h4 className="my-4">{t('subtitle3')}</h4>
              <p className="mb-3 text-justify">
                <strong>10.</strong> {t('paragraph10')}
              </p>
              <p className="mb-3 text-justify">
                {t('paragraph10A', { company: APP_META_TITLE })}
              </p>
              <h4 className="my-4">{t('subtitle4')}</h4>
              <p className="mb-3 text-justify">
                <strong>11.</strong>{' '}
                {t.rich('paragraph11', {
                  company: APP_META_TITLE,
                  b: (chunk) => <strong>{chunk}</strong>,
                })}
              </p>
              <p className="mb-3 text-justify">
                <strong>12.</strong>{' '}
                {t('paragraph12', { company: APP_META_TITLE })}
              </p>
              <h4 className="my-4">{t('subtitle5')}</h4>
              <p className="mb-3 text-justify">
                <strong>13.</strong>{' '}
                {t('paragraph13', { company: APP_META_TITLE })}
              </p>
              <p className="mb-3 text-justify">
                <strong>14.</strong> {t('paragraph14')}
              </p>
              <h4 className="my-4">{t('subtitle6')}</h4>
              <p className="mb-3 text-justify">
                <strong>15.</strong> {t('paragraph15')}
              </p>
              <p className="mb-3 text-justify">
                <strong>16.</strong> {t('paragraph16', { email: EMAIL })}
              </p>
              <p className="mb-3 text-justify">
                <strong>17.</strong> {t('paragraph17')}
              </p>
              <h4 className="my-4">{t('subtitle7')}</h4>
              <p className="mb-3 text-justify">
                <strong>18.</strong> {t('paragraph18')}
              </p>
              <p className="mb-3 text-justify">
                <strong>19.</strong> {t('paragraph19')}
              </p>
              <p className="mb-3 text-justify">
                <strong>20.</strong> {t('paragraph20')}
              </p>
              <p className="mb-3 text-justify">
                <strong>21.</strong> {t('paragraph21')}
              </p>
              <p className="mb-3 text-justify">
                <strong>22.</strong> {t('paragraph22', { email: EMAIL })}
              </p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
