'use client'

import { useQuery } from '@apollo/client'
import { DateTime } from 'luxon'
import { useLocale, useTranslations } from 'next-intl'
import { useEffect, useState } from 'react'
import { toast } from 'react-toastify'
import {
  Button,
  CardBody,
  CardFooter,
  Col,
  Nav,
  NavItem,
  NavLink,
  Row,
  TabContent,
  TabPane,
} from 'reactstrap'

import Can from '@/components/backend/Guards/Can'
import Avatar from '@/components/common/Avatar'
import SpinnerBoxed from '@/components/common/SpinnerBoxed'
import { APP_DATETIME_FORMAT } from '@/environment'
import { FIND_USER } from '@/graphql/users'
import { useAuth } from '@/hooks'
import { useRouter } from '@/navigation'

type UsersViewProps = {
  id: string
}

const defaultImageUrl = '/assets/images/user/user.jpg'

export const UsersView = ({ id }: UsersViewProps) => {
  const [activeTab, setActiveTab] = useState('tab1')
  const [imgSrc, setImgSrc] = useState(defaultImageUrl)
  const [displayError, setDisplayError] = useState(false)
  const { data, loading, error } = useQuery(FIND_USER, {
    variables: { id },
    fetchPolicy: 'no-cache',
  })

  const t = useTranslations()
  const router = useRouter()
  const locale = useLocale()
  const { timezone } = useAuth()

  useEffect(() => {
    if (error && !displayError) {
      toast.error(error.message)
      setDisplayError(true)
    }
    if (data?.findByIdUser?.image) setImgSrc(data?.findByIdUser?.image)
  }, [displayError, error, data?.findByIdUser?.image])

  return loading ? (
    <SpinnerBoxed color="default" type="border" />
  ) : (
    <>
      <CardBody>
        <Nav tabs>
          <NavItem>
            <NavLink
              className={activeTab === 'tab1' ? 'active' : ''}
              onClick={() => setActiveTab('tab1')}
            >
              {t('dataName', { name: t('user'), gender: 'male' })}
            </NavLink>
          </NavItem>
          <NavItem>
            <NavLink
              className={activeTab === 'tab2' ? 'active' : ''}
              onClick={() => setActiveTab('tab2')}
            >
              {t('roles')}
            </NavLink>
          </NavItem>
          <NavItem>
            <NavLink
              className={activeTab === 'tab3' ? 'active' : ''}
              onClick={() => setActiveTab('tab3')}
            >
              {t('permissions')}
            </NavLink>
          </NavItem>
        </Nav>
        <TabContent activeTab={activeTab}>
          <TabPane tabId="tab1" className="px-3 pt-4">
            <Row className="g-3 mb-2 py-3 border-bottom d-flex align-items-center">
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4">
                    {t('id')}:
                  </Col>
                  <Col lg={8} className="px-4 f-s-italic fw-bold">
                    {data?.findByIdUser?.id}
                  </Col>
                </Row>
              </Col>
              <Col lg={6} sm={12} className="d-flex justify-content-start">
                <Avatar
                  image={imgSrc}
                  name={data?.findByIdUser?.name ?? ''}
                  size={100}
                  rounded
                  className="me-2 img-fluid"
                />
              </Col>
            </Row>

            <Row className="g-3 mb-2 py-3 border-bottom">
              <Col lg={12}>
                <Row>
                  <Col lg={2} className="fw-bold px-4">
                    {t('account')}:
                  </Col>
                  <Col lg={10} className="px-4 f-s-italic">
                    {data?.findByIdUser?.account?.systemName ??
                      data?.findByIdUser?.account?.tradingName ??
                      t('none', {
                        gender: 'female',
                      })}
                  </Col>
                </Row>
              </Col>
            </Row>

            <Row className="g-3 mb-2 py-3 border-bottom">
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4 required">
                    {t('name')}:
                  </Col>
                  <Col lg={8} className="px-4 f-s-italic">
                    {data?.findByIdUser?.name}
                  </Col>
                </Row>
              </Col>
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4 required">
                    {t('systemName')}:
                  </Col>
                  <Col lg={8} className="px-4 f-s-italic">
                    {data?.findByIdUser?.systemName}
                  </Col>
                </Row>
              </Col>
            </Row>

            <Row className="g-3 mb-2 py-3 border-bottom">
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4 required">
                    {t('email')}:
                  </Col>
                  <Col lg={8} className="px-4 f-s-italic">
                    {data?.findByIdUser?.email}
                  </Col>
                </Row>
              </Col>
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4 required">
                    {t('active')}:
                  </Col>
                  <Col lg={8} className="px-4">
                    <span
                      className={`badge p-2 badge-${data?.findByIdUser?.isActive ? 'success' : 'danger'}`}
                    >
                      {data?.findByIdUser?.isActive ? t('yes') : t('no')}
                    </span>
                  </Col>
                </Row>
              </Col>
            </Row>

            <Row className="g-3 mb-2 py-3 border-bottom">
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4 required">
                    {t('language')}:
                  </Col>
                  <Col lg={8} className="px-4 f-s-italic">
                    {t(data?.findByIdUser?.language)}
                  </Col>
                </Row>
              </Col>
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4 required">
                    {t('timezone')}:
                  </Col>
                  <Col lg={8} className="px-4 f-s-italic">
                    {data?.findByIdUser?.timezone.name}
                  </Col>
                </Row>
              </Col>
            </Row>

            <Row className="g-3 py-3">
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4 required">
                    {t('createdAt')}:
                  </Col>
                  <Col lg={8} className="px-4 f-s-italic">
                    {data?.findByIdUser.createdAt &&
                      DateTime.fromISO(data.findByIdUser.createdAt)
                        .setZone(timezone)
                        .setLocale(locale)
                        .toFormat(APP_DATETIME_FORMAT)}
                  </Col>
                </Row>
              </Col>
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4 required">
                    {t('updatedAt')}:
                  </Col>
                  <Col lg={8} className="px-4 f-s-italic">
                    {data?.findByIdUser.updatedAt &&
                      DateTime.fromISO(data.findByIdUser.updatedAt)
                        .setZone(timezone)
                        .setLocale(locale)
                        .toFormat(APP_DATETIME_FORMAT)}
                  </Col>
                </Row>
              </Col>
            </Row>

            {data?.findByIdUser?.deletedAt && (
              <Row className="g-3 mt-2 pt-3 border-top">
                <Col lg={6} sm={12}>
                  <Row>
                    <Col lg={4} className="fw-bold px-4 required">
                      {t('deletedAt')}:
                    </Col>
                    <Col lg={8} className="px-4 f-s-italic">
                      {DateTime.fromISO(data.findByIdUser.deletedAt)
                        .setZone(timezone)
                        .setLocale(locale)
                        .toFormat(APP_DATETIME_FORMAT)}
                    </Col>
                  </Row>
                </Col>
                <Col lg={6} sm={12}></Col>
              </Row>
            )}
          </TabPane>

          <TabPane tabId="tab2">
            <Col lg={12}>
              <Row className="g-3 pt-4">
                {data?.findByIdUser?.roles &&
                data.findByIdUser.roles.length > 0 ? (
                  data.findByIdUser.roles.map(({ id, name }) => {
                    return (
                      <Col lg={3} key={id}>
                        <span className={`badge badge-gray-500 p-2`}>
                          {name}
                        </span>
                      </Col>
                    )
                  })
                ) : (
                  <Col lg={12}>
                    <div className="alert alert-light-warning rounded-3 text-center">
                      {t('noItemsFound')}
                    </div>
                  </Col>
                )}
              </Row>
            </Col>
          </TabPane>

          <TabPane tabId="tab3">
            <Col lg={12}>
              <Row className="g-3 pt-4">
                {data?.findByIdUser?.claims &&
                data.findByIdUser.claims.length > 0 ? (
                  data.findByIdUser.claims.map((claim) => {
                    const [subject, action] = claim.split(':')
                    const label = `${t(action)} ${t(subject)}`

                    return (
                      <Col lg={3} key={claim}>
                        <span className={`badge badge-gray-500 p-2`}>
                          {label}
                        </span>
                      </Col>
                    )
                  })
                ) : (
                  <Col lg={12}>
                    <div className="alert alert-light-warning rounded-3 text-center">
                      {t('noItemsFound')}
                    </div>
                  </Col>
                )}
              </Row>
            </Col>
          </TabPane>
        </TabContent>
      </CardBody>
      <CardFooter>
        <Row>
          <Col lg={6} sm={12}>
            <Button
              color="gray"
              className="px-4"
              type="button"
              disabled={loading}
              onClick={() => router.push('/backend/system/users')}
            >
              <i className="fa fa-arrow-left me-2"></i>
              {t('back')}
            </Button>
          </Col>
          <Col lg={6} sm={12} className="justify-content-lg-end d-flex">
            <Can action="Update" subject="User">
              <Button
                color="success"
                className="px-4"
                type="button"
                disabled={loading}
                onClick={() => router.push(`/backend/system/users/edit/${id}`)}
              >
                <i className="fa fa-pencil-square-o me-2"></i>
                {t('edit')}
              </Button>
            </Can>
          </Col>
        </Row>
      </CardFooter>
    </>
  )
}
