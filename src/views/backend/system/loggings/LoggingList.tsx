'use client'

import { useLazyQuery, useMutation, useQuery } from '@apollo/client'
import { trim } from 'lodash'
import { useTranslations } from 'next-intl'
import { Fragment, useCallback, useEffect, useMemo, useState } from 'react'
import { TableColumn } from 'react-data-table-component'
import { HiDotsVertical } from '@react-icons/all-files/hi/HiDotsVertical'
import { HiLightningBolt } from '@react-icons/all-files/hi/HiLightningBolt'
import { HiOutlineEye } from '@react-icons/all-files/hi/HiOutlineEye'
import { HiOutlineTrash } from '@react-icons/all-files/hi/HiOutlineTrash'
import Select from 'react-select'
import { toast } from 'react-toastify'
import { Tooltip } from 'react-tooltip'
import {
  Button,
  Card,
  CardBody,
  Col,
  Container,
  Dropdown,
  DropdownItem,
  DropdownMenu,
  DropdownToggle,
  FormGroup,
  Input,
  InputGroup,
  Label,
  Row,
} from 'reactstrap'
import Swal from 'sweetalert2'

import Can from '@/components/backend/Guards/Can'
import CanAny from '@/components/backend/Guards/CanAny'
import CommonCardHeading from '@/components/common/CommonCardHeading'
import Table, { SelectChangeState } from '@/components/common/Table'
import api from '@/configs/axios'
import { APP_PAGINATION } from '@/environment'
import {
  DELETE_LOGGING,
  DELETE_LOGGINGS,
  PAGINATED_LOGGINGS,
} from '@/graphql/logging'
import { useAbility } from '@/hooks'
import { Link } from '@/navigation'
import { ActionEnum } from '@/types/action'
import {
  FindManyInput,
  ModeEnum,
  OrderByEnum,
  PaginatedInput,
} from '@/types/common'
import { Logging } from '@/types/logging'
import { Subjects } from '@/types/subject'
import { User, UserToken } from '@/types/user'

import { ExpandedComponent } from './ExpandedComponent'

export const LoggingList = () => {
  const t = useTranslations()
  const ability = useAbility()
  const pageTitle = t('listName', { name: t('loggings') })
  const pageDescription = t('seeInformationAboutName', {
    gender: 'male',
    name: t('loggings').toLowerCase(),
  })

  const defaultVariables: PaginatedInput = useMemo(() => {
    const variables: PaginatedInput = {
      page: 1,
      perPage: APP_PAGINATION,
      orderBy: { createdAt: OrderByEnum.DESC },
      where: {
        AND: [],
        OR: [],
      },
    }

    if (variables.where?.AND?.length === 0) delete variables.where.AND
    if (variables.where?.OR?.length === 0) delete variables.where.OR

    return variables
  }, [])

  const [userId, setUserId] = useState('')
  const [users, setUsers] = useState<User[]>([])
  const [tokens, setTokens] = useState<
    Array<{ user: User; tokens: UserToken[] }>
  >([])
  const [filterText, setFilterText] = useState('')
  const [rows, setRows] = useState<Logging[]>([])
  const [selectedRows, setSelectedRows] = useState<Logging[]>([])
  const [totalRows, setTotalRows] = useState(0)
  const [toggleCleared, setToggleCleared] = useState(false)
  const [displayError, setDisplayError] = useState(true)
  const [dropdownOpen, setDropdownOpen] = useState<any>({})
  const [variables, setVariables] = useState<PaginatedInput>(defaultVariables)

  const [deleteLogging] = useMutation(DELETE_LOGGING)
  const [deleteLoggings] = useMutation(DELETE_LOGGINGS)
  const { data, loading, error } = useQuery(PAGINATED_LOGGINGS, {
    fetchPolicy: 'no-cache',
    variables,
  })
  const [handleLoggings] = useLazyQuery(PAGINATED_LOGGINGS, {
    fetchPolicy: 'no-cache',
    variables,
  })

  useMemo(
    async () =>
      await api
        .post<User[]>('/backend/users', {
          where: {
            deletedAt: null,
          },
          orderBy: { systemName: OrderByEnum.ASC },
        })
        .then(({ data }) => {
          setUsers(data)
          setTokens(
            data
              .filter(({ tokens }) => tokens.length)
              .map((user) => ({ user, tokens: user.tokens }))
          )
        })
        .catch((error) => toast.error(error?.response?.data?.message)),
    []
  )

  const toggleDropdown = useCallback(
    (id: number) => {
      setDropdownOpen({
        ...Object.keys(dropdownOpen).reduce(
          (acc, cur) => ({ ...acc, [cur]: false }),
          {}
        ),
        [id]: !dropdownOpen[id],
      })
    },
    [dropdownOpen]
  )

  const handleSelectedRows = useCallback(
    (state: SelectChangeState<Logging>) => {
      setSelectedRows(state.selectedRows)
    },
    []
  )

  const handleSearch = useCallback(
    async (e?: React.FormEvent<HTMLFormElement> | null, _userId?: number) => {
      if (typeof e !== 'undefined') {
        e?.preventDefault()
      }

      const where: FindManyInput = {
        AND: [],
        OR: [],
      }

      if (_userId || userId) {
        const user = users.find((user) => user.id === (_userId ?? userId))

        if (tokens.length) {
          const userToken = tokens.find(
            ({ user }) => user.id === (_userId ?? userId)
          )

          if (userToken) {
            where.AND.push({
              tokenId: {
                in: userToken.tokens.map((token) => token.id),
              },
            })
          } else if (user) {
            where.AND.push({
              userId: {
                equals: user.id,
              },
            })
          }
        } else if (user) {
          where.AND.push({
            userId: {
              equals: user.id,
            },
          })
        }
      }

      if (trim(filterText)) {
        where.OR.push({
          operation: {
            contains: filterText,
            mode: ModeEnum.INSENSITIVE,
          },
        })
        where.OR.push({
          endpoint: {
            contains: filterText,
            mode: ModeEnum.INSENSITIVE,
          },
        })
        where.OR.push({
          ipAddress: {
            equals: filterText,
          },
        })
        where.OR.push({
          userAgent: {
            contains: filterText,
            mode: ModeEnum.INSENSITIVE,
          },
        })
        where.OR.push({
          requestData: {
            contains: filterText,
            mode: ModeEnum.INSENSITIVE,
          },
        })
        where.OR.push({
          responseData: {
            contains: filterText,
            mode: ModeEnum.INSENSITIVE,
          },
        })
      }

      if (where.AND?.length === 0) delete where.AND
      if (where.OR?.length === 0) delete where.OR

      setVariables({ ...variables, page: 1, where })
    },
    [filterText, tokens, users, variables, userId]
  )

  const handleSetUser = useCallback(
    async (option: any) => {
      setUserId(option?.value ?? '')
      await handleSearch(null, option?.value ?? '')
    },
    [handleSearch]
  )

  const handleReset = useCallback(async () => {
    setFilterText('')
    setVariables(defaultVariables)
    await handleLoggings().then(({ data }) => {
      setRows(data?.paginatedLogging.data || [])
      setDropdownOpen(
        data?.paginatedLogging.data.reduce(
          (acc, cur) => ({ ...acc, [cur.id]: false }),
          {}
        )
      )
      setTotalRows(data?.paginatedLogging.meta.total || 0)
    })
  }, [defaultVariables, handleLoggings])

  const contextActions = useMemo(() => {
    const handleDelete = () => {
      Swal.fire({
        title: t('confirmation'),
        text: t('itemsDeleteConfirm'),
        icon: 'question',
        showCancelButton: true,
        confirmButtonText: t('yes'),
        cancelButtonText: t('no'),
      }).then(async ({ isConfirmed }) => {
        if (isConfirmed) {
          await deleteLoggings({
            variables: { ids: selectedRows.map((row) => row.id) },
          })
            .then(async ({ data }) => {
              const res = data?.deleteManyLogging ?? false
              if (res) {
                await handleReset()
                toast.success(t('itemsDeleteSuccess'))
                setToggleCleared(!toggleCleared)
              } else {
                toast.error(t('itemsDeleteError'))
              }
            })
            .catch((err) => toast.error(err?.message ?? t('itemsDeleteError')))
        } else {
          setToggleCleared(!toggleCleared)
        }
      })
    }

    return (
      <>
        <Button
          type="button"
          onClick={handleDelete}
          color="danger"
          className="px-3 text-white"
          disabled={loading}
        >
          <i className="fa fa-trash pe-2" aria-hidden="true"></i>
          {t('delete')}
        </Button>
      </>
    )
  }, [loading, t, selectedRows, handleReset, toggleCleared, deleteLoggings])

  const handleDelete = useCallback(
    async (
      e: React.MouseEvent<HTMLAnchorElement> | React.MouseEvent<HTMLElement>,
      id: number
    ) => {
      e?.preventDefault()

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
                await handleReset()
                toast.success(t('itemDeleteSuccess'))
              } else {
                toast.error(t('itemDeleteError'))
              }
            })
            .catch((err) => toast.error(err?.message ?? t('itemDeleteError')))
        }
      })
    },
    [t, deleteLogging, handleReset]
  )

  const columns: TableColumn<Logging>[] = useMemo(
    () =>
      [
        {
          name: t('method'),
          sortable: true,
          sortField: 'method',
          selector: (row) => row.method,
        },
        {
          name: t('operation'),
          sortable: true,
          sortField: 'operation',
          selector: (row) => row.operation ?? row.endpoint,
        },
        {
          name: t('ipAddress'),
          sortable: true,
          sortField: 'ipAddress',
          selector: (row) => row.ipAddress,
        },
        {
          name: t('user'),
          sortable: false,
          cell: (row) => (
            <Fragment>{row.user?.systemName ?? row.user?.name ?? '-'}</Fragment>
          ),
        },
        {
          name: <HiLightningBolt className="h-4 w-4" />,
          center: true,
          width: '70px',
          cell: (row) => (
            <CanAny
              acls={[
                { action: ActionEnum.Read, subject: Subjects.Logging },
                { action: ActionEnum.Delete, subject: Subjects.Logging },
                { action: ActionEnum.Manage, subject: Subjects.Logging },
              ]}
            >
              <Dropdown
                isOpen={dropdownOpen[row.id] ?? false}
                toggle={() => toggleDropdown(row.id)}
              >
                <DropdownToggle
                  color="transparent"
                  className="btn-dotted d-flex align-items-start justify-content-center"
                  caret={false}
                >
                  <HiDotsVertical />
                </DropdownToggle>
                <DropdownMenu flip>
                  <DropdownItem
                    header
                    className="d-flex justify-content-center align-items-center"
                  >
                    {t('actions')}
                  </DropdownItem>
                  <Can action={ActionEnum.Read} subject={Subjects.Logging}>
                    <DropdownItem
                      className="d-flex justify-content-start align-items-center"
                      href={`/backend/system/loggings/view/${row.id}`}
                      tag={Link}
                    >
                      <HiOutlineEye className="me-2" />
                      {t('viewName', { name: t('logging') })}
                    </DropdownItem>
                  </Can>
                  <CanAny
                    acls={[
                      { action: ActionEnum.Delete, subject: Subjects.Logging },
                      { action: ActionEnum.Manage, subject: Subjects.Logging },
                    ]}
                  >
                    <DropdownItem
                      className="d-flex justify-content-start align-items-center"
                      href="#!"
                      onClick={(e) => handleDelete(e, row.id)}
                      tag={Link}
                    >
                      <HiOutlineTrash className="me-2" />
                      {t('deleteName', { name: t('logging') })}
                    </DropdownItem>
                  </CanAny>
                </DropdownMenu>
              </Dropdown>
            </CanAny>
          ),
        },
      ] as unknown as TableColumn<Logging>[],
    [t, dropdownOpen, toggleDropdown, handleDelete]
  )

  const subHeaderComponentMemo = useMemo(() => {
    return (
      <form onSubmit={handleSearch} className="dataTables_filter">
        <Container fluid className="px-0 gx-5">
          <Row>
            <Col lg={4} sm={12} className="text-lg-start"></Col>
            <Col lg={4} sm={12}>
              <FormGroup row>
                <Label for="user" lg={4}>
                  {t('user')}:
                </Label>
                <Col lg={8} className="justify-content-start text-start">
                  <Select
                    id="user"
                    name="user"
                    placeholder="--"
                    className="react-select-container"
                    classNamePrefix="react-select"
                    isDisabled={loading}
                    onChange={(newValue) => handleSetUser(newValue as any)}
                    options={
                      [{ label: '--', value: '' }].concat(
                        users.map((user) => ({
                          label:
                            `${user.name}` +
                            (user.account
                              ? ` (${user.account.systemName ?? user.account.tradingName})`
                              : ''),
                          value: user.id,
                        })) as any
                      ) as any[]
                    }
                    noOptionsMessage={({ inputValue }) =>
                      !trim(inputValue as string) ? '' : t('noResultsFound')
                    }
                  />
                </Col>
              </FormGroup>
            </Col>
            <Col lg={4} sm={12}>
              <FormGroup row className="justify-content-lg-end">
                <Label for="search" lg={4}>
                  {t('search')}:
                </Label>
                <Col lg={8}>
                  <InputGroup>
                    <Input
                      id="search"
                      onChange={(e) => setFilterText(e.target.value)}
                      type="text"
                      disabled={loading}
                      value={filterText}
                    />
                    <Button
                      type="submit"
                      color="primary"
                      disabled={loading}
                      className="text-white"
                    >
                      <i className="fa fa-search" />
                    </Button>
                  </InputGroup>
                </Col>
              </FormGroup>
            </Col>
          </Row>
        </Container>
      </form>
    )
  }, [filterText, handleSearch, handleSetUser, loading, t, users])

  useEffect(() => {
    if (error && displayError) {
      toast.error(error.message)
      setDisplayError(false)
    }
    if (data) {
      setRows(data.paginatedLogging.data)
      setDropdownOpen(
        data.paginatedLogging.data.reduce(
          (acc, cur) => ({ ...acc, [cur.id]: false }),
          {}
        )
      )
      setTotalRows(data.paginatedLogging.meta.total)
    }
  }, [error, data, pageTitle, pageDescription, displayError])

  return (
    <>
      <Card>
        <CommonCardHeading smallHeading={pageTitle} span={pageDescription} />
        <CardBody>
          <Tooltip id="tooltip" />
          <div className="table-responsive">
            <Table
              data={rows}
              columns={columns}
              defaultSortFieldId={1}
              disabled={loading}
              progressPending={loading}
              contextActions={contextActions}
              selectableRows
              onSelectedRowsChange={handleSelectedRows}
              clearSelectedRows={toggleCleared}
              paginationPerPage={variables.perPage}
              highlightOnHover
              selectableRowDisabled={() =>
                !ability.can(ActionEnum.Delete, Subjects.Logging)
              }
              pagination
              paginationServer
              paginationTotalRows={totalRows}
              onChangePage={(page) => {
                setVariables({ ...variables, page })
              }}
              onChangeRowsPerPage={(currentRowsPerPage, currentPage) => {
                setVariables({
                  ...variables,
                  page: currentPage,
                  perPage: currentRowsPerPage,
                })
              }}
              onSort={(selectedColumn, sortDirection) => {
                if (selectedColumn.sortField)
                  setVariables({
                    ...variables,
                    orderBy: {
                      [selectedColumn.sortField]: sortDirection.toUpperCase(),
                    },
                  })
              }}
              className="display dataTable"
              subHeader
              subHeaderComponent={subHeaderComponentMemo}
              expandableRows
              expandableRowsComponent={ExpandedComponent}
              spinner={{ type: 'border' }}
              noDataComponentText={t('noDataNameText', {
                name: t('logging').toLowerCase(),
                gender: 'male',
              })}
            />
          </div>
        </CardBody>
      </Card>
    </>
  )
}
