'use client'

import { useLazyQuery, useMutation } from '@apollo/client'
import { yupResolver } from '@hookform/resolvers/yup'
import { trim } from 'lodash'
import { useTranslations } from 'next-intl'
import {
  ChangeEvent,
  Fragment,
  useCallback,
  useEffect,
  useMemo,
  useState,
} from 'react'
import { Controller, useForm } from 'react-hook-form'
import Select from 'react-select'
import { toast } from 'react-toastify'
import {
  Button,
  CardBody,
  CardFooter,
  Col,
  FormFeedback,
  FormGroup,
  Input,
  Label,
  Row,
} from 'reactstrap'
import { v4 as uuidv4 } from 'uuid'
import * as yup from 'yup'

import Avatar from '@/components/common/Avatar'
import FinishForm from '@/components/common/NumberingWizard/FinishForm'
import StepperHorizontal from '@/components/common/NumberingWizard/StepperHorizontal'
import { EMAIL_REGEX, PASSWORD_STRENGTH_REGEX } from '@/configs'
import api from '@/configs/axios'
import { APP_TIMEZONE } from '@/environment'
import { CREATE_USER, FIND_USER, UPDATE_USER } from '@/graphql/users'
import { useAuth } from '@/hooks'
import { useRouter } from '@/navigation'
import { Account } from '@/types/account'
import { Claim } from '@/types/claim'
import {
  OrderByEnum,
  ReactSelectType,
  WhereAndOrderInput,
} from '@/types/common'
import { UserTypeEnum } from '@/types/enums'
import { LanguageEnum } from '@/types/language'
import { Role } from '@/types/role'
import { ProtectedSubjectsEnum } from '@/types/subject'
import { Timezone } from '@/types/timezone'
import { CreateUserInput, UpdateUserInput, User, UserInput } from '@/types/user'
import { setFormValues, validateFormValue } from '@/utils/helpers'

type UsersFormProps = {
  id?: number
  mode: 'create' | 'update'
}

type FormData = {
  accountId: number | null
  name: string
  systemName: string
  email: string
  password: string
  passwordConfirmation: string
  imageFile: File | null
  removeImage: boolean
  language: string
  timezoneId: number | null
  isActive: boolean
  isSuperAdmin: boolean
  claims: string[]
  roles: number[]
}

const defaultValues: FormData = {
  accountId: null,
  name: '',
  systemName: '',
  email: '',
  password: '',
  passwordConfirmation: '',
  removeImage: false,
  isActive: false,
  imageFile: null,
  isSuperAdmin: false,
  language: LanguageEnum.pt_BR,
  timezoneId: null,
  claims: [],
  roles: [],
}

export const UsersForm = ({ id, mode }: UsersFormProps) => {
  const t = useTranslations()
  const { jwt } = useAuth()

  const [password, setPassword] = useState('')

  const basePasswordSchema = useMemo(
    () => ({
      password: yup.string().trim().optional(),
      passwordConfirmation: yup.string().trim().optional(),
    }),
    []
  )
  const baseSchema = useMemo(
    () => ({
      name: yup
        .string()
        .required(t('propertyRequired', { property: t('name') })),
      systemName: yup
        .string()
        .required(t('propertyRequired', { property: t('systemName') })),
      email: yup
        .string()
        .email(t('propertyEmail', { property: t('email') }))
        .required(t('propertyRequired', { property: t('email') }))
        .matches(
          new RegExp(EMAIL_REGEX),
          t('propertyEmail', { property: t('email') })
        )
        .default(''),
      language: yup
        .string()
        .nullable()
        .required(t('propertyRequired', { property: t('language') }))
        .default(null),
      timezoneId: yup
        .number()
        .nullable()
        .required(t('propertyRequired', { property: t('timezone') }))
        .default(null),
      accountId: yup.number().optional().default(null),
      removeImage: yup.boolean().optional().default(false),
      isActive: yup.boolean().optional().default(false),
      isSuperAdmin: yup.boolean().optional().default(false),
      languages: yup.array().optional().default([]),
      timezones: yup.array().optional().default([]),
      accounts: yup.array().optional().default([]),
      claims: yup.array().optional().default([]),
      roles: yup.array().optional().default([]),
    }),
    [t]
  )
  const passwordSchema = useMemo(
    () => ({
      password: yup
        .string()
        .trim()
        .required(t('propertyRequired', { property: t('password') }))
        .matches(
          new RegExp(PASSWORD_STRENGTH_REGEX),
          t('propertyStrength', { property: t('password') })
        ),
      passwordConfirmation: yup
        .string()
        .required(
          t('propertyRequired', { property: t('passwordConfirmation') })
        )
        .oneOf(
          [yup.ref('password')],
          t('propertyMatch', {
            property: t('passwordConfirmation'),
            match: t('password'),
          })
        ),
    }),
    [t]
  )
  const schema = useMemo(() => {
    if (password?.trim() !== '' || mode === 'create') {
      return yup.object().shape({
        ...baseSchema,
        ...passwordSchema,
      })
    } else {
      return yup.object().shape({
        ...baseSchema,
        ...basePasswordSchema,
      })
    }
  }, [basePasswordSchema, baseSchema, password, passwordSchema, mode])

  const [level, setLevel] = useState(1)
  const [disabled, setDisabled] = useState(false)
  const [imgSrc, setImgSrc] = useState('')
  const [imageFile, setImageFile] = useState<File | null>(null)
  const [showPassword, setShowPassword] = useState(false)
  const [showPasswordConfirmation, setShowPasswordConfirmation] =
    useState(false)
  const [user, setUser] = useState<User>()
  const [languages] = useState<ReactSelectType[]>([
    {
      label: t('portuguese'),
      value: 'pt-br',
    },
  ])
  const [timezones, setTimezones] = useState<ReactSelectType[]>([])
  const [accounts, setAccounts] = useState<ReactSelectType[]>([])
  const [claims, setClaims] = useState<Claim[]>([])
  const [roles, setRoles] = useState<Role[]>([])

  const [getUser] = useLazyQuery(FIND_USER, {
    variables: { id: id ?? 0 },
    fetchPolicy: 'no-cache',
  })
  const [createUser] = useMutation(CREATE_USER)
  const [updateUser] = useMutation(UPDATE_USER, { fetchPolicy: 'no-cache' })

  const router = useRouter()

  const {
    control,
    setValue,
    getValues,
    handleSubmit,
    formState: { errors },
  } = useForm({
    defaultValues,
    mode: 'onBlur',
    resolver: yupResolver(schema as unknown as yup.ObjectSchema<FormData>),
  })

  const getAccounts = useCallback(
    async (data?: any) =>
      await api
        .post<Account[]>(`/backend/accounts`, data)
        .then(({ data }) =>
          setAccounts(
            data?.map(
              ({ id, systemName }) =>
                ({
                  value: id,
                  label: systemName,
                }) as unknown as ReactSelectType
            )
          )
        )
        .catch((error) => toast.error(error?.response?.data?.message)),
    []
  )

  const getClaims = useCallback(
    async () =>
      await api
        .get<Claim[]>(`/backend/claims`)
        .then(({ data }) => setClaims(data))
        .catch((error) => toast.error(error?.response?.data?.message)),
    []
  )

  const getRoles = useCallback(
    async () =>
      await api
        .get<Role[]>(`/backend/roles?token=${uuidv4()}`)
        .then(({ data }) => setRoles(data))
        .catch((error) => toast.error(error?.response?.data?.message)),
    []
  )

  const getTimezones = useCallback(
    async () =>
      await api
        .get<Timezone[]>(`/backend/timezones`)
        .then(({ data }) => {
          setTimezones(
            data?.map(
              ({ id, name }) =>
                ({
                  value: id,
                  label: name,
                }) as unknown as ReactSelectType
            )
          )
          if (mode === 'create' && data) {
            const defaultTimezone = data.find(
              ({ code }) => code === APP_TIMEZONE
            )
            if (defaultTimezone) {
              setValue('timezoneId', defaultTimezone.id)
            }
          }
        })
        .catch((error) => toast.error(error?.response?.data?.message)),
    [mode, setValue]
  )

  const handleUser = useCallback(async () => {
    setDisabled(true)

    let accountsVariables: WhereAndOrderInput = {
      where: {
        deletedAt: null,
      },
      orderBy: { systemName: OrderByEnum.ASC },
    }

    if (!jwt?.sa && jwt?.accountId) {
      accountsVariables = {
        where: {
          id: jwt.accountId,
          deletedAt: null,
        },
        orderBy: { systemName: OrderByEnum.ASC },
      }
    }

    if (mode === 'update') {
      const [user] = await Promise.all([
        getUser(),
        getTimezones(),
        getAccounts(accountsVariables),
        getClaims(),
        getRoles(),
      ])

      if (user?.data?.findByIdUser) {
        const { findByIdUser } = user.data
        const { roles, image, ...rest } = findByIdUser

        if (image) setImgSrc(image)

        setUser(findByIdUser)
        setFormValues(setValue, {
          ...rest,
          password: '',
          passwordConfirmation: '',
          roles: roles?.map((role) => role.id),
        })
      }

      if (user?.error) toast.error(user?.error?.message)
    } else {
      await Promise.all([
        getTimezones(),
        getAccounts(accountsVariables),
        getClaims(),
        getRoles(),
      ])
    }
    setDisabled(false)
  }, [
    mode,
    getUser,
    getAccounts,
    getClaims,
    getRoles,
    getTimezones,
    setValue,
    jwt,
  ])

  const onSubmit = async (form: FormData) => {
    const data: UserInput = {
      accountId: jwt?.accountId ?? form.accountId ?? null,
      name: form.name,
      systemName: form.systemName,
      email: form.email,
      password: form.password ?? null,
      isActive: form.isActive ?? false,
      isSuperAdmin: form.isSuperAdmin ?? false,
      imageFile: form.removeImage ? null : imageFile,
      removeImage: form.removeImage ?? false,
      language: form.language ?? LanguageEnum.pt_BR,
      timezoneId: form.timezoneId ?? null,
      type: UserTypeEnum.CREDENTIAL,
      claims: form.claims,
      roles: form.roles,
    }

    if (mode === 'create') {
      const variables: CreateUserInput = {
        data,
      }
      setDisabled(true)

      await createUser({ variables })
        .then(({ data }) => {
          if (data?.createUser) {
            toast.success(t('nameCreatedSuccess', { name: t('user') }))

            router.push(`/backend/system/users`)
          } else {
            toast.error(
              t('nameCreatedError', { name: t('user').toLowerCase() })
            )
          }
        })
        .catch((error) =>
          toast.error(
            error?.message ??
              t('nameCreatedError', { name: t('user').toLowerCase() })
          )
        )
    } else if (mode === 'update') {
      const variables: UpdateUserInput = {
        data: {
          id: id ?? 0,
          ...data,
        },
      }

      setDisabled(true)
      await updateUser({ variables })
        .then(({ data }) => {
          if (data?.updateUser) {
            toast.success(t('nameUpdatedSuccess', { name: t('user') }))
            const { updateUser } = data
            const { roles, ...rest } = updateUser
            setFormValues(setValue, {
              ...rest,
              roles: roles?.map((role) => role.id),
            })
            router.push(`/backend/system/users`)
          } else {
            toast.error(
              t('nameUpdatedError', { name: t('user').toLowerCase() })
            )
          }
        })
        .catch((error) =>
          toast.error(
            error?.message ??
              t('nameUpdatedError', { name: t('user').toLowerCase() })
          )
        )
    }

    setDisabled(false)
  }

  const handleBackButton = () => {
    if (level === 2) {
      setLevel(level - 1)
    }
    if (level === 3) {
      setLevel(level - 1)
    }
    if (level === 4) {
      setLevel(level - 1)
    }
  }

  const handleNextButton = () => {
    if (mode === 'create') {
      if (
        validateFormValue(getValues('name'), errors.name) &&
        validateFormValue(getValues('systemName'), errors.systemName) &&
        validateFormValue(getValues('email'), errors.email) &&
        validateFormValue(getValues('language'), errors.language) &&
        validateFormValue(getValues('timezoneId'), errors.timezoneId) &&
        validateFormValue(getValues('password'), errors.password) &&
        validateFormValue(
          getValues('passwordConfirmation'),
          errors.passwordConfirmation
        ) &&
        level === 1
      ) {
        setLevel(2)
      } else if (
        !Boolean(errors.name) &&
        !Boolean(errors.systemName) &&
        !Boolean(errors.email) &&
        !Boolean(errors.language) &&
        !Boolean(errors.timezoneId) &&
        !Boolean(errors.password) &&
        !Boolean(errors.passwordConfirmation) &&
        level === 1
      ) {
        toast.error(t('typeFieldsRequired'))
      }
    } else {
      if (
        validateFormValue(getValues('name'), errors.name) &&
        validateFormValue(getValues('systemName'), errors.systemName) &&
        validateFormValue(getValues('email'), errors.email) &&
        validateFormValue(getValues('language'), errors.language) &&
        validateFormValue(getValues('timezoneId'), errors.timezoneId) &&
        level === 1
      ) {
        setLevel(2)
      } else if (
        !Boolean(errors.name) &&
        !Boolean(errors.systemName) &&
        !Boolean(errors.email) &&
        !Boolean(errors.language) &&
        !Boolean(errors.timezoneId) &&
        level === 1
      ) {
        toast.error(t('typeFieldsRequired'))
      }
    }

    if (level === 2) {
      setLevel(3)
    } else if (level === 3) {
      setLevel(4)
    } else {
      const error = Object.values(errors).shift()
      error?.message && toast.error(error.message)
    }
  }

  const handleInputImageChange = (file: ChangeEvent) => {
    const reader = new FileReader()
    const { files } = file.target as HTMLInputElement
    if (files && files.length !== 0) {
      reader.onload = () => setImgSrc(reader.result as string)
      reader.readAsDataURL(files[0])
      setImageFile(files[0])

      // @ts-ignore
      setValue('removeImage', false)
    }
  }

  useEffect(() => {
    handleUser()
  }, [handleUser])

  return (
    <>
      <form noValidate autoComplete="on" onSubmit={handleSubmit(onSubmit)}>
        <CardBody className="basic-wizard important-validation">
          <StepperHorizontal
            disabled={disabled}
            mode={mode}
            level={level}
            setLevel={setLevel}
            steps={[
              t('dataName', { name: t('user'), gender: 'male' }),
              t('roles'),
              t('permissions'),
            ]}
          />
          <div className="step-content">
            <div className={`pt-3 ${level === 1 ? '' : 'd-none'}`}>
              <Row className="g-3 mb-3 d-flex align-items-center">
                <Col lg={6} sm={12}>
                  <Row>
                    <Col lg={9} sm={12}>
                      <FormGroup row>
                        <Label for="imageFile" lg={5} className="fw-bold px-3">
                          {t('photo')}:
                        </Label>
                        <Col lg={7} className="pe-3">
                          <Input
                            id="imageFile"
                            name="imageFile"
                            type="file"
                            accept="image/png, image/jpeg"
                            onChange={handleInputImageChange}
                          />
                        </Col>
                      </FormGroup>
                    </Col>
                    <Col lg={3} sm={12} className="d-flex justify-content-end">
                      <Avatar
                        image={imgSrc}
                        name={
                          getValues('name')?.trim()?.length > 2
                            ? getValues('name')?.trim()
                            : 'U'
                        }
                        size={70}
                        rounded
                        className="me-2 img-fluid"
                      />
                    </Col>
                  </Row>
                </Col>
                <Col lg={6} sm={12}>
                  <FormGroup
                    row
                    className={
                      mode === 'update' && imgSrc !== '' ? '' : 'd-none'
                    }
                  >
                    <Label for="removeImage" lg={4} className="fw-bold px-3">
                      {t('removePhoto')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <Controller
                        control={control}
                        name="removeImage"
                        disabled={disabled}
                        rules={{ required: true }}
                        render={({
                          field: { name, value, onChange, ...rest },
                        }) => (
                          <Input
                            id={name}
                            type="checkbox"
                            value="true"
                            checked={value}
                            invalid={Boolean(errors.removeImage)}
                            onChange={(e) => {
                              setImageFile(null)
                              setImgSrc('')
                              onChange(e.target.checked)
                              const imgFile = document?.getElementById(
                                'imageFile'
                              ) as HTMLInputElement
                              if (imgFile) imgFile.value = ''
                            }}
                            {...rest}
                          />
                        )}
                      />
                      {errors.removeImage && (
                        <FormFeedback>
                          {errors.removeImage.message}
                        </FormFeedback>
                      )}
                    </Col>
                  </FormGroup>
                </Col>
              </Row>
              <Row className="g-3">
                <Col lg={6} sm={12}>
                  <FormGroup row>
                    <Label for="name" lg={4} className="fw-bold px-3 required">
                      {t('name')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <Controller
                        control={control}
                        name="name"
                        disabled={disabled}
                        rules={{ required: true }}
                        render={({ field: { name, ...rest } }) => (
                          <Input
                            id={name}
                            placeholder={t('typeName', {
                              gender: 'male',
                              name: t('name').toLowerCase(),
                            })}
                            invalid={Boolean(errors.name)}
                            {...rest}
                          />
                        )}
                      />
                      {errors.name && (
                        <FormFeedback>{errors.name.message}</FormFeedback>
                      )}
                    </Col>
                  </FormGroup>
                </Col>
                <Col lg={6} sm={12}>
                  <FormGroup row>
                    <Label for="email" lg={4} className="fw-bold px-3 required">
                      {t('email')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <Controller
                        control={control}
                        name="email"
                        disabled={disabled}
                        rules={{ required: true }}
                        render={({ field: { name, ...rest } }) => (
                          <Input
                            id={name}
                            type="email"
                            autoComplete="off"
                            placeholder={t('typeName', {
                              gender: 'male',
                              name: t('email').toLowerCase(),
                            })}
                            invalid={Boolean(errors.email)}
                            {...rest}
                          />
                        )}
                      />
                      {errors.email && (
                        <FormFeedback>{errors.email.message}</FormFeedback>
                      )}
                    </Col>
                  </FormGroup>
                </Col>
              </Row>
              <Row className="g-3">
                <Col lg={6} sm={12}>
                  <FormGroup row>
                    <Label
                      for="systemName"
                      lg={4}
                      className="fw-bold px-3 required"
                    >
                      {t('systemName')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <Controller
                        control={control}
                        name="systemName"
                        disabled={disabled}
                        rules={{ required: true }}
                        render={({ field: { name, ...rest } }) => (
                          <Input
                            id={name}
                            autoComplete="off"
                            placeholder={t('typeName', {
                              gender: 'male',
                              name: t('systemName').toLowerCase(),
                            })}
                            invalid={Boolean(errors.systemName)}
                            {...rest}
                          />
                        )}
                      />
                      {errors.systemName && (
                        <FormFeedback>{errors.systemName.message}</FormFeedback>
                      )}
                    </Col>
                  </FormGroup>
                </Col>
                {jwt?.sa ? (
                  <Col lg={6} sm={12}>
                    <FormGroup row>
                      <Label for="accountId" lg={4} className="fw-bold px-3">
                        {t('account')}:
                      </Label>
                      <Col lg={8} className="px-3">
                        <Controller
                          name="accountId"
                          control={control}
                          disabled={disabled}
                          rules={{ required: true }}
                          render={({
                            field: { name, onChange, value, disabled, ref },
                          }) => (
                            <div
                              className={`select2-input ${errors.accountId && errors.accountId.message ? 'has-error' : ''}`}
                            >
                              <Select
                                ref={ref}
                                name={name}
                                placeholder={t('selectOne')}
                                className="react-select-container"
                                classNamePrefix="react-select"
                                isDisabled={disabled ?? false}
                                onChange={(newValue) =>
                                  onChange(newValue?.value ?? '')
                                }
                                value={accounts.find(
                                  (option) => option.value === value
                                )}
                                options={
                                  [{ label: t('selectOne'), value: '' }].concat(
                                    accounts as any
                                  ) as any
                                }
                                noOptionsMessage={({ inputValue }) =>
                                  !trim(inputValue as string)
                                    ? ''
                                    : t('noResultsFound')
                                }
                              />
                              <input
                                type="hidden"
                                className={
                                  errors.accountId && errors.accountId.message
                                    ? 'is-invalid'
                                    : ''
                                }
                                value={value ?? ''}
                              />
                              <FormFeedback>
                                {errors.accountId && errors.accountId.message}
                              </FormFeedback>
                            </div>
                          )}
                        />
                      </Col>
                    </FormGroup>
                  </Col>
                ) : (
                  <Col lg={6} sm={12}>
                    <FormGroup row>
                      <Label for="accountId" lg={4} className="fw-bold px-3">
                        {t('account')}:
                      </Label>
                      <Col lg={8} className="px-3">
                        <Controller
                          control={control}
                          name="accountId"
                          disabled={disabled}
                          rules={{ required: true }}
                          render={({ field: { name, ...rest } }) => (
                            <Input
                              id={name}
                              readOnly
                              invalid={Boolean(errors.accountId)}
                              {...rest}
                              value={
                                accounts.find(
                                  ({ value }) =>
                                    value === user?.accountId ||
                                    value === jwt?.accountId
                                )?.label ?? t('none', { gender: 'female' })
                              }
                            />
                          )}
                        />
                        {errors.accountId && (
                          <FormFeedback>
                            {errors.accountId.message}
                          </FormFeedback>
                        )}
                      </Col>
                    </FormGroup>
                  </Col>
                )}
              </Row>
              <Row className="g-3">
                <Col lg={6} sm={12}>
                  <FormGroup row>
                    <Label
                      for="language"
                      lg={4}
                      className="fw-bold px-3 required"
                    >
                      {t('language')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <Controller
                        name="language"
                        control={control}
                        disabled={disabled}
                        rules={{ required: true }}
                        render={({
                          field: { name, onChange, value, disabled, ref },
                        }) => (
                          <div
                            className={`select2-input ${errors.language && errors.language.message ? 'has-error' : ''}`}
                          >
                            <Select
                              ref={ref}
                              name={name}
                              placeholder={t('selectOne')}
                              className="react-select-container"
                              classNamePrefix="react-select"
                              isDisabled={disabled ?? false}
                              onChange={(newValue) =>
                                onChange(newValue?.value ?? '')
                              }
                              value={languages.find(
                                (option) => option.value === value
                              )}
                              options={
                                [{ label: t('selectOne'), value: '' }].concat(
                                  languages as any
                                ) as any
                              }
                              noOptionsMessage={({ inputValue }) =>
                                !trim(inputValue as string)
                                  ? ''
                                  : t('noResultsFound')
                              }
                            />
                            <input
                              type="hidden"
                              className={
                                errors.language && errors.language.message
                                  ? 'is-invalid'
                                  : ''
                              }
                              value={value ?? ''}
                            />
                            <FormFeedback>
                              {errors.language && errors.language.message}
                            </FormFeedback>
                          </div>
                        )}
                      />
                    </Col>
                  </FormGroup>
                </Col>
                <Col lg={6} sm={12}>
                  <FormGroup row>
                    <Label
                      for="timezoneId"
                      lg={4}
                      className="fw-bold px-3 required"
                    >
                      {t('timezone')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <Controller
                        name="timezoneId"
                        control={control}
                        disabled={disabled}
                        rules={{ required: true }}
                        render={({
                          field: { name, onChange, value, disabled, ref },
                        }) => (
                          <div
                            className={`select2-input ${errors.timezoneId && errors.timezoneId.message ? 'has-error' : ''}`}
                          >
                            <Select
                              ref={ref}
                              name={name}
                              placeholder={t('selectOne')}
                              className="react-select-container"
                              classNamePrefix="react-select"
                              isDisabled={disabled ?? false}
                              onChange={(newValue) =>
                                onChange(newValue?.value ?? '')
                              }
                              value={timezones.find(
                                (option) => option.value === value
                              )}
                              options={
                                [{ label: t('selectOne'), value: '' }].concat(
                                  timezones as any
                                ) as any
                              }
                              noOptionsMessage={({ inputValue }) =>
                                !trim(inputValue as string)
                                  ? ''
                                  : t('noResultsFound')
                              }
                            />
                            <input
                              type="hidden"
                              className={
                                errors.timezoneId && errors.timezoneId.message
                                  ? 'is-invalid'
                                  : ''
                              }
                              value={value ?? ''}
                            />
                            <FormFeedback>
                              {errors.timezoneId && errors.timezoneId.message}
                            </FormFeedback>
                          </div>
                        )}
                      />
                    </Col>
                  </FormGroup>
                </Col>
              </Row>
              <Row className="g-3">
                <Col lg={6} sm={12}>
                  <FormGroup row>
                    <Label
                      for="password"
                      lg={4}
                      className={`fw-bold px-3 ${
                        mode === 'create'
                          ? 'required'
                          : password.trim() !== ''
                            ? 'required'
                            : ''
                      }`}
                    >
                      {t('password')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <div className="form-input position-relative">
                        <Controller
                          control={control}
                          name="password"
                          disabled={disabled}
                          rules={{ required: true }}
                          render={({ field: { name, onChange, ...rest } }) => (
                            <Input
                              id={name}
                              type={showPassword ? 'text' : 'password'}
                              autoComplete="off"
                              onChange={(e) => {
                                onChange(e.target.value)
                                setPassword(e.target.value)
                              }}
                              invalid={Boolean(errors.password)}
                              {...rest}
                            />
                          )}
                        />
                        {errors.password && (
                          <FormFeedback>{errors.password.message}</FormFeedback>
                        )}
                        <div className="show-hide">
                          <span
                            onClick={() => setShowPassword(!showPassword)}
                            className={!showPassword ? 'show' : ''}
                          >
                            {showPassword ? t('hide') : t('show')}
                          </span>
                        </div>
                      </div>
                    </Col>
                  </FormGroup>
                </Col>
                <Col lg={6} sm={12}>
                  <FormGroup row>
                    <Label
                      for="passwordConfirmation"
                      lg={4}
                      className={`fw-bold px-3 ${
                        mode === 'create'
                          ? 'required'
                          : password.trim() !== ''
                            ? 'required'
                            : ''
                      }`}
                    >
                      {t('confirmPass')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <div className="form-input position-relative">
                        <Controller
                          control={control}
                          name="passwordConfirmation"
                          disabled={disabled}
                          rules={{ required: true }}
                          render={({ field: { name, value, ...rest } }) => (
                            <Input
                              id={name}
                              type={
                                showPasswordConfirmation ? 'text' : 'password'
                              }
                              value={value ?? ''}
                              autoComplete="off"
                              invalid={Boolean(errors.passwordConfirmation)}
                              {...rest}
                            />
                          )}
                        />
                        {errors.passwordConfirmation && (
                          <FormFeedback>
                            {errors.passwordConfirmation.message}
                          </FormFeedback>
                        )}
                        <div className="show-hide">
                          <span
                            onClick={() =>
                              setShowPasswordConfirmation(
                                !showPasswordConfirmation
                              )
                            }
                            className={!showPasswordConfirmation ? 'show' : ''}
                          >
                            {showPasswordConfirmation ? t('hide') : t('show')}
                          </span>
                        </div>
                      </div>
                    </Col>
                  </FormGroup>
                </Col>
              </Row>
              <Row className="g-3">
                <Col lg={6} sm={12}>
                  <FormGroup row>
                    <Label for="isActive" lg={4} className="fw-bold px-3">
                      {t('active')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <Controller
                        name="isActive"
                        control={control}
                        disabled={disabled}
                        render={({ field: { name, value, ...rest } }) => (
                          <Input
                            id={name}
                            type="checkbox"
                            value="true"
                            checked={value}
                            {...rest}
                          />
                        )}
                      />
                    </Col>
                  </FormGroup>
                </Col>
                <Col lg={6} sm={12}>
                  {jwt?.sa && (
                    <FormGroup row>
                      <Label for="isSuperAdmin" lg={4} className="fw-bold px-3">
                        {t('superAdmin')}:
                      </Label>
                      <Col lg={8} className="px-3">
                        <Controller
                          name="isSuperAdmin"
                          control={control}
                          disabled={disabled}
                          render={({ field: { name, value, ...rest } }) => (
                            <Input
                              id={name}
                              type="checkbox"
                              value="true"
                              checked={value}
                              {...rest}
                            />
                          )}
                        />
                      </Col>
                    </FormGroup>
                  )}
                </Col>
              </Row>
            </div>

            <Row className={`pt-3 ${level === 2 ? '' : 'd-none'}`}>
              <Col>
                <FormGroup row>
                  <Controller
                    name="roles"
                    control={control}
                    disabled={disabled}
                    render={({ field: { onChange, value } }) => (
                      <>
                        {roles?.map(({ id, accountId, name, slug }) => {
                          if (!jwt?.sa && !jwt?.roles.includes('admin')) {
                            if (
                              !jwt?.roles?.includes(slug) &&
                              !value.includes(id)
                            ) {
                              return <Fragment key={id}></Fragment>
                            } else if (
                              !jwt?.roles?.includes(slug) &&
                              value.includes(id)
                            ) {
                              return (
                                <Label
                                  key={id}
                                  for={`role-${id}`}
                                  lg={4}
                                  sm={6}
                                >
                                  <Input
                                    id={`role-${id}`}
                                    type="checkbox"
                                    className="me-2"
                                    value={id}
                                    disabled
                                    checked
                                  />
                                  {name}
                                </Label>
                              )
                            }
                          } else if (!jwt?.sa && jwt?.roles.includes('admin')) {
                            if (
                              !jwt?.roles?.includes(slug) &&
                              !value.includes(id) &&
                              jwt?.accountId !== accountId
                            ) {
                              return <Fragment key={id}></Fragment>
                            } else if (
                              !jwt?.roles?.includes(slug) &&
                              value.includes(id) &&
                              jwt?.accountId !== accountId
                            ) {
                              return (
                                <Label
                                  key={id}
                                  for={`role-${id}`}
                                  lg={4}
                                  sm={6}
                                >
                                  <Input
                                    id={`role-${id}`}
                                    type="checkbox"
                                    className="me-2"
                                    value={id}
                                    disabled
                                    checked
                                  />
                                  {name}
                                </Label>
                              )
                            }
                          }

                          return (
                            <Label key={id} for={`role-${id}`} lg={4} sm={6}>
                              <Input
                                id={`role-${id}`}
                                type="checkbox"
                                className="me-2"
                                value={id}
                                checked={value && value?.includes(id)}
                                onChange={(e) => {
                                  if (e.target.checked) {
                                    value && onChange([...value, id])
                                  } else {
                                    value &&
                                      onChange(
                                        value.filter((val) => val !== id)
                                      )
                                  }
                                }}
                              />
                              {name}
                            </Label>
                          )
                        })}
                      </>
                    )}
                  />
                </FormGroup>
              </Col>
            </Row>

            <Row className={`pt-3 ${level === 3 ? '' : 'd-none'}`}>
              <Col>
                <FormGroup row>
                  <Controller
                    name="claims"
                    control={control}
                    disabled={disabled}
                    render={({ field: { onChange, value } }) => (
                      <>
                        {claims?.map(({ action, subject }) => {
                          const key = `${subject}:${action}`
                          const label = `${t(action)} ${t(subject)}`

                          if (!jwt?.sa && !jwt?.roles.includes('admin')) {
                            if (!jwt?.claims?.includes(key))
                              return <Fragment key={key}></Fragment>
                            if (
                              Object.keys(ProtectedSubjectsEnum).includes(
                                subject
                              )
                            )
                              return <Fragment key={key}></Fragment>
                          } else if (!jwt?.sa && jwt?.roles.includes('admin')) {
                            if (
                              Object.keys(ProtectedSubjectsEnum).includes(
                                subject
                              )
                            )
                              return <Fragment key={key}></Fragment>
                          }

                          return (
                            <Label key={key} for={key} lg={4} sm={6}>
                              <Input
                                id={key}
                                type="checkbox"
                                className="me-2"
                                value={key}
                                checked={value && value?.includes(key)}
                                onChange={(e) => {
                                  if (e.target.checked) {
                                    value && onChange([...value, key])
                                  } else {
                                    value &&
                                      onChange(
                                        value.filter((val) => val !== key)
                                      )
                                  }
                                }}
                              />
                              {label}
                            </Label>
                          )
                        })}
                      </>
                    )}
                  />
                </FormGroup>
              </Col>
            </Row>

            <div className={`pt-3 ${level === 4 ? '' : 'd-none'}`}>
              <FinishForm />
            </div>
          </div>
        </CardBody>
        <CardFooter>
          <Row>
            <Col lg={6} sm={12}>
              <Button
                color="gray"
                className="px-3"
                type="button"
                disabled={disabled}
                onClick={() => router.push('/backend/system/users')}
              >
                <i className="fa fa-times-circle me-2"></i>
                {t('cancel')}
              </Button>
            </Col>
            <Col lg={6} sm={12} className="justify-content-lg-end d-flex">
              <div className="wizard-footer d-flex gap-2 justify-content-end">
                {level > 1 && (
                  <Button
                    type="button"
                    color="secondary"
                    className="px-3"
                    disabled={disabled}
                    onClick={handleBackButton}
                  >
                    <i className="fa fa-arrow-left me-2"></i>
                    {t('back')}
                  </Button>
                )}
                {level < 4 && (
                  <Button
                    type="button"
                    color="primary"
                    className="px-3"
                    disabled={disabled}
                    onClick={handleNextButton}
                  >
                    <i className="fa fa-arrow-right me-2"></i>
                    {t('next')}
                  </Button>
                )}
                {level === 4 && (
                  <Button
                    type="submit"
                    disabled={disabled}
                    color="primary"
                    className="px-3"
                  >
                    <i className="fa fa-save me-2"></i>
                    {t('save')}
                  </Button>
                )}
              </div>
            </Col>
          </Row>
        </CardFooter>
      </form>
    </>
  )
}
