'use client'

import { useLazyQuery, useMutation, useQuery } from '@apollo/client'
import { trim } from 'lodash'
import { useTranslations } from 'next-intl'
import { Fragment, useCallback, useEffect, useMemo, useState } from 'react'
import { TableColumn } from 'react-data-table-component'
import { HiDotsVertical } from 'react-icons/hi'
import { HiBolt, HiEye, HiTrash } from 'react-icons/hi2'
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

import { Can } from '@/components/backend/Guards/Can'
import { CanAny } from '@/components/backend/Guards/CanAny'
import CommonCardHeading from '@/components/common/CommonCardHeading'
import Table, { SelectChangeState } from '@/components/common/Table'
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

  const toggleDropdown = useCallback(
    (id: string) => {
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
    async (e?: React.FormEvent<HTMLFormElement>) => {
      if (typeof e !== 'undefined') {
        e?.preventDefault()
      }

      const where: FindManyInput = {
        AND: [],
        OR: [],
      }

      if (trim(filterText)) {
        where.OR.push({
          operation: {
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
    [filterText, variables]
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
      id: string
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
          selector: (row) => row.operation,
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
          name: <HiBolt className="h-4 w-4" />,
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
                      <HiEye className="me-2" />
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
                      <HiTrash className="me-2" />
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
            <Col lg={6} sm={12} className="text-lg-start"></Col>
            <Col lg={6} sm={12}>
              <FormGroup row className="justify-content-lg-end">
                <Label htmlFor="search" lg={3}>
                  {t('search')}:
                </Label>
                <Col lg={6}>
                  <InputGroup>
                    <Input
                      id="search"
                      onChange={(e) => setFilterText(e.target.value)}
                      type="text"
                      value={filterText}
                    />
                    <Button
                      type="submit"
                      color="primary"
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
  }, [filterText, handleSearch, t])

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
              spinner={{ type: 'grow' }}
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
