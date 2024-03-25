'use client'

import { useQuery } from '@apollo/client'
import { trim } from 'lodash'
import { useTranslations } from 'next-intl'
import { useCallback, useEffect, useMemo, useState } from 'react'
import { TableColumn } from 'react-data-table-component'
import { toast } from 'react-toastify'
import { Tooltip } from 'react-tooltip'
import {
  Button,
  Card,
  CardBody,
  Col,
  Container,
  FormGroup,
  Input,
  InputGroup,
  Label,
  Row,
} from 'reactstrap'

import CommonCardHeading from '@/components/common/CommonCardHeading'
import Table from '@/components/common/Table'
import { APP_PAGINATION } from '@/environment'
import { FIND_MANY_CLAIMS } from '@/graphql/claims'
import { useAbility } from '@/hooks'
import { ActionEnum } from '@/types/action'
import { Claim } from '@/types/claim'
import { FindManyInput, WhereInput } from '@/types/common'
import { Subjects } from '@/types/subject'

export const PermissionsList = () => {
  const t = useTranslations()
  const ability = useAbility()
  const pageTitle = t('listName', { name: t('permissions') })
  const pageDescription = t('seeInformationAboutName', {
    gender: 'female',
    name: t('permissions').toLowerCase(),
  })

  const defaultVariables: WhereInput = useMemo(() => {
    const variables: WhereInput = {
      where: {},
    }

    return variables
  }, [])

  const [filterText, setFilterText] = useState('')
  const [rows, setRows] = useState<Claim[]>([])
  const [totalRows, setTotalRows] = useState(0)
  const [displayError, setDisplayError] = useState(true)
  const [variables, setVariables] = useState<WhereInput>(defaultVariables)

  const { data, loading, error } = useQuery(FIND_MANY_CLAIMS, {
    fetchPolicy: 'no-cache',
    variables,
  })
  const handleSearch = useCallback(
    async (e?: React.FormEvent<HTMLFormElement>) => {
      if (typeof e !== 'undefined') {
        e?.preventDefault()
      }

      let where: FindManyInput = {}

      if (trim(filterText)) where = { subject: filterText }

      setVariables({ ...variables, where })
    },
    [filterText, variables]
  )

  const columns: TableColumn<Claim>[] = useMemo(
    () => [
      {
        name: t('module'),
        sortable: true,
        sortField: 'subject',
        selector: (row) => row.subject,
      },
      {
        name: t('action'),
        sortable: true,
        sortField: 'action',
        selector: (row) => row.action,
      },
    ],
    [t]
  )

  const subHeaderComponentMemo = useMemo(() => {
    return (
      <form onSubmit={handleSearch} className="dataTables_filter">
        <Container fluid className="px-0 gx-5">
          <Row>
            <Col lg={12} sm={12}>
              <FormGroup row className="justify-content-lg-end">
                <Label htmlFor="search" lg={2}>
                  {t('module')}:
                </Label>
                <Col lg={3}>
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
      setRows(
        data.findManyClaim.map(
          ({ action, subject }) =>
            ({
              action: `${t(action)} (${action})`,
              subject: `${t(subject)} (${subject})`,
            }) as Claim
        )
      )
      setTotalRows(data.findManyClaim.length ?? 0)
    }
  }, [error, data, pageTitle, pageDescription, displayError, t])

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
              selectableRows={false}
              paginationPerPage={APP_PAGINATION}
              highlightOnHover
              selectableRowDisabled={() =>
                !ability.can(ActionEnum.Delete, Subjects.Role)
              }
              pagination
              paginationServer={false}
              paginationTotalRows={totalRows}
              className="display dataTable"
              subHeader
              subHeaderComponent={subHeaderComponentMemo}
              spinner={{ type: 'grow' }}
              noDataComponentText={t('noDataNameText', {
                name: t('permission').toLowerCase(),
                gender: 'female',
              })}
            />
          </div>
        </CardBody>
      </Card>
    </>
  )
}
