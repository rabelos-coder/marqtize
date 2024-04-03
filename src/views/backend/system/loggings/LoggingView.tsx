'use client'

import { useMutation, useQuery } from '@apollo/client'
import { DateTime } from 'luxon'
import { useLocale, useTranslations } from 'next-intl'
import { useCallback, useEffect, useState } from 'react'
import { LuDownload } from 'react-icons/lu'
import ReactJson from 'react-json-view'
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
import Swal from 'sweetalert2'

import { Can } from '@/components/backend/Guards/Can'
import { Avatar } from '@/components/common/Avatar'
import { SpinnerBoxed } from '@/components/common/SpinnerBoxed'
import { APP_DATETIME_FORMAT } from '@/environment'
import { DELETE_LOGGING, FIND_LOGGING } from '@/graphql/logging'
import { useAppSelector, useAuth } from '@/hooks'
import { useRouter } from '@/navigation'

type LoggingViewProps = {
  id: string
}

const defaultImageUrl = '/assets/images/user/user.jpg'

export const LoggingView = ({ id }: LoggingViewProps) => {
  const [activeTab, setActiveTab] = useState('tab1')
  const [imgSrc, setImgSrc] = useState(defaultImageUrl)
  const [displayError, setDisplayError] = useState(false)

  const [deleteLogging] = useMutation(DELETE_LOGGING)
  const { data, loading, error } = useQuery(FIND_LOGGING, {
    variables: { id },
    fetchPolicy: 'no-cache',
  })

  const t = useTranslations()
  const router = useRouter()
  const locale = useLocale()
  const { timezone } = useAuth()
  const { theme } = useAppSelector((state) => state.theme)

  const handleDownload = (content, fileName) => {
    const element = document.createElement('a')
    element.setAttribute(
      'href',
      'data:application/json;charset=utf-8,' + encodeURIComponent(content)
    )
    element.setAttribute('download', fileName)

    element.style.display = 'none'
    document.body.appendChild(element)

    element.click()

    document.body.removeChild(element)
  }

  const handleDelete = useCallback(async () => {
    Swal.fire({
      title: t('confirmation'),
      text: t('itemDeleteConfirm'),
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: t('yes'),
      cancelButtonText: t('no'),
    }).then(async ({ isConfirmed }) => {
      if (isConfirmed) {
        await deleteLogging({ variables: { id } })
          .then(async ({ data }) => {
            const res = data?.deleteLogging ?? false
            if (res) {
              toast.success(t('itemDeleteSuccess'))
              router.push('/backend/system/loggings')
            } else {
              toast.error(t('itemDeleteError'))
            }
          })
          .catch((err) => toast.error(err?.message ?? t('itemDeleteError')))
      }
    })
  }, [t, deleteLogging, id, router])

  useEffect(() => {
    if (error && !displayError) {
      toast.error(error.message)
      setDisplayError(true)
    }
    if (data?.findByIdLogging?.user?.image)
      setImgSrc(data?.findByIdLogging?.user?.image)
  }, [displayError, error, data?.findByIdLogging?.user?.image])

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
              {t('dataName', { name: t('logging'), gender: 'male' })}
            </NavLink>
          </NavItem>
          <NavItem>
            <NavLink
              className={activeTab === 'tab2' ? 'active' : ''}
              onClick={() => setActiveTab('tab2')}
            >
              {t('requestData')}
            </NavLink>
          </NavItem>
          <NavItem>
            <NavLink
              className={activeTab === 'tab3' ? 'active' : ''}
              onClick={() => setActiveTab('tab3')}
            >
              {t('responseData')}
            </NavLink>
          </NavItem>
          {data?.findByIdLogging?.user && (
            <NavItem>
              <NavLink
                className={activeTab === 'tab4' ? 'active' : ''}
                onClick={() => setActiveTab('tab4')}
              >
                {t('user')}
              </NavLink>
            </NavItem>
          )}
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
                    {data?.findByIdLogging?.id}
                  </Col>
                </Row>
              </Col>
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4">
                    {t('method')}:
                  </Col>
                  <Col lg={8} className="px-4 f-s-italic">
                    {data?.findByIdLogging?.method}
                  </Col>
                </Row>
              </Col>
            </Row>

            <Row className="g-3 mb-2 py-3 border-bottom">
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4">
                    {t('operation')}:
                  </Col>
                  <Col lg={8} className="px-4 f-s-italic">
                    {data?.findByIdLogging?.operation
                      ? data?.findByIdLogging?.operation
                      : t('none', {
                          gender: 'female',
                        })}
                  </Col>
                </Row>
              </Col>
              <Col lg={6} sm={12}>
                <Row>
                  <Col lg={4} className="fw-bold px-4">
                    {t('endpoint')}:
                  </Col>
                  <Col lg={8} className="px-4 f-s-italic">
                    {data?.findByIdLogging?.endpoint
                      ? data?.findByIdLogging?.endpoint
                      : t('none', {
                          gender: 'male',
                        })}
                  </Col>
                </Row>
              </Col>
            </Row>

            <Row className="g-3 mb-2 py-3 border-bottom">
              <Col lg={12}>
                <Row>
                  <Col lg={2} className="fw-bold px-4 required">
                    {t('userAgent')}:
                  </Col>
                  <Col lg={10} className="px-4 f-s-italic">
                    {data?.findByIdLogging?.userAgent}
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
                    {data?.findByIdLogging.createdAt &&
                      DateTime.fromISO(data.findByIdLogging.createdAt)
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
                    {data?.findByIdLogging.updatedAt &&
                      DateTime.fromISO(data.findByIdLogging.updatedAt)
                        .setZone(timezone)
                        .setLocale(locale)
                        .toFormat(APP_DATETIME_FORMAT)}
                  </Col>
                </Row>
              </Col>
            </Row>
          </TabPane>

          <TabPane tabId="tab2">
            <Col lg={12}>
              <Row className="g-3 pt-4">
                {data?.findByIdLogging?.requestData ? (
                  <>
                    <Col lg={12} className="d-flex justify-content-lg-end">
                      <Button
                        color="success"
                        onClick={() =>
                          handleDownload(
                            `${data?.findByIdLogging?.requestData}`,
                            `Request-${data?.findByIdLogging?.id}.json`
                          )
                        }
                      >
                        <LuDownload className="me-2" />
                        {t('download')}
                      </Button>
                    </Col>
                    <Col lg={12}>
                      <ReactJson
                        name={null}
                        indentWidth={2}
                        displayDataTypes={false}
                        theme={
                          theme === 'dark'
                            ? 'summerfruit'
                            : 'summerfruit:inverted'
                        }
                        style={{
                          padding: '20px',
                          borderRadius: '10px',
                        }}
                        src={JSON.parse(
                          data?.findByIdLogging?.requestData ?? {}
                        )}
                      />
                    </Col>
                  </>
                ) : (
                  <Col lg={12}>
                    <div className="alert alert-light-warning rounded-3 text-center">
                      {t('noData')}
                    </div>
                  </Col>
                )}
              </Row>
            </Col>
          </TabPane>

          <TabPane tabId="tab3">
            <Col lg={12}>
              <Row className="g-3 pt-4">
                {data?.findByIdLogging?.responseData ? (
                  <>
                    <Col lg={12} className="d-flex justify-content-lg-end">
                      <Button
                        color="success"
                        onClick={() =>
                          handleDownload(
                            `${data?.findByIdLogging?.responseData}`,
                            `Response-${data?.findByIdLogging?.id}.json`
                          )
                        }
                      >
                        <LuDownload className="me-2" />
                        {t('download')}
                      </Button>
                    </Col>
                    <Col lg={12}>
                      <ReactJson
                        name={null}
                        indentWidth={2}
                        displayDataTypes={false}
                        theme={
                          theme === 'dark'
                            ? 'summerfruit'
                            : 'summerfruit:inverted'
                        }
                        style={{
                          padding: '20px',
                          borderRadius: '10px',
                        }}
                        src={JSON.parse(
                          data?.findByIdLogging?.responseData ?? {}
                        )}
                      />
                    </Col>
                  </>
                ) : (
                  <Col lg={12}>
                    <div className="alert alert-light-warning rounded-3 text-center">
                      {t('noData')}
                    </div>
                  </Col>
                )}
              </Row>
            </Col>
          </TabPane>

          {data?.findByIdLogging?.user && (
            <TabPane tabId="tab4">
              <Row className="g-3 mb-2 py-3 border-bottom d-flex align-items-center">
                <Col lg={6} sm={12}>
                  <Row>
                    <Col lg={4} className="fw-bold px-4">
                      {t('id')}:
                    </Col>
                    <Col lg={8} className="px-4 f-s-italic fw-bold">
                      {data?.findByIdLogging?.user.id}
                    </Col>
                  </Row>
                </Col>
                <Col lg={6} sm={12} className="d-flex justify-content-start">
                  <Avatar
                    image={imgSrc}
                    name={data?.findByIdLogging?.user.name ?? 'User'}
                    size={100}
                    rounded
                    className="me-2 img-fluid"
                  />
                </Col>
              </Row>

              <Row className="g-3 mb-2 py-3 border-bottom">
                <Col lg={6} sm={12}>
                  <Row>
                    <Col lg={4} className="fw-bold px-4">
                      {t('name')}:
                    </Col>
                    <Col lg={8} className="px-4 f-s-italic">
                      {data?.findByIdLogging?.user.name}
                    </Col>
                  </Row>
                </Col>
                <Col lg={6} sm={12}>
                  <Row>
                    <Col lg={4} className="fw-bold px-4">
                      {t('systemName')}:
                    </Col>
                    <Col lg={8} className="px-4 f-s-italic">
                      {data?.findByIdLogging?.user.systemName}
                    </Col>
                  </Row>
                </Col>
              </Row>

              <Row className="g-3 mb-2 py-3 border-bottom">
                <Col lg={6} sm={12}>
                  <Row>
                    <Col lg={4} className="fw-bold px-4">
                      {t('email')}:
                    </Col>
                    <Col lg={8} className="px-4 f-s-italic">
                      {data?.findByIdLogging?.user.email}
                    </Col>
                  </Row>
                </Col>
                <Col lg={6} sm={12}>
                  <Row>
                    <Col lg={4} className="fw-bold px-4">
                      {t('account')}:
                    </Col>
                    <Col lg={8} className="px-4 f-s-italic">
                      {data?.findByIdLogging?.user.account?.systemName ??
                        data?.findByIdLogging?.user.account?.tradingName ??
                        t('none', { gender: 'female' })}
                    </Col>
                  </Row>
                </Col>
              </Row>

              <Row className="g-3 mb-2 py-3">
                <Col lg={6} sm={12}>
                  <Row>
                    <Col lg={4} className="fw-bold px-4">
                      {t('type')}:
                    </Col>
                    <Col lg={8} className="px-4 f-s-italic">
                      {t(data?.findByIdLogging?.user.type.toLowerCase())}
                    </Col>
                  </Row>
                </Col>
              </Row>
            </TabPane>
          )}
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
              onClick={() => router.push('/backend/system/loggings')}
            >
              <i className="fa fa-arrow-left me-2"></i>
              {t('back')}
            </Button>
          </Col>
          <Col lg={6} sm={12} className="justify-content-lg-end d-flex">
            <Can action="Delete" subject="Logging">
              <Button
                color="danger"
                className="px-4"
                type="button"
                disabled={loading}
                onClick={handleDelete}
              >
                <i className="fa fa-trash me-2"></i>
                {t('delete')}
              </Button>
            </Can>
          </Col>
        </Row>
      </CardFooter>
    </>
  )
}
