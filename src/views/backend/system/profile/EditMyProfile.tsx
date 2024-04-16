'use client'

import { useMutation } from '@apollo/client'
import { yupResolver } from '@hookform/resolvers/yup'
import { useTranslations } from 'next-intl'
import { ChangeEvent, useEffect, useMemo, useState } from 'react'
import { Controller, useForm } from 'react-hook-form'
import { toast } from 'react-toastify'
import {
  Button,
  Card,
  CardBody,
  Col,
  FormFeedback,
  FormGroup,
  Input,
  Label,
  Row,
  Tooltip,
} from 'reactstrap'
import Sawl from 'sweetalert2'
import * as yup from 'yup'

import Avatar from '@/components/common/Avatar'
import CommonCardHeading from '@/components/common/CommonCardHeading'
import { EMAIL_REGEX, PASSWORD_STRENGTH_REGEX } from '@/configs'
import { UPDATE_PROFILE } from '@/graphql/auth'
import { useAppDispatch, useAuth } from '@/hooks'
import { setUser } from '@/store/slices/authSlice'
import { UpdateProfileInput } from '@/types/auth'

type FormData = {
  email: string
  password?: string | null
  passwordConfirmation?: string | null
  imageFile?: File | null
}

const EditMyProfile = () => {
  const t = useTranslations()

  const [password, setPassword] = useState('')
  const [tooltipOpen1, setTooltipOpen1] = useState(false)
  const [tooltipOpen2, setTooltipOpen2] = useState(false)

  const toggle1 = () => setTooltipOpen1(!tooltipOpen1)
  const toggle2 = () => setTooltipOpen2(!tooltipOpen2)

  const basePasswordSchema = useMemo(
    () => ({
      password: yup.string().trim().optional(),
      passwordConfirmation: yup.string().trim().optional(),
    }),
    []
  )
  const baseSchema = useMemo(
    () => ({
      email: yup
        .string()
        .trim()
        .email(t('propertyEmail', { property: t('email') }))
        .required(t('propertyRequired', { property: t('email') }))
        .matches(
          new RegExp(EMAIL_REGEX),
          t('propertyEmail', { property: t('email') })
        ),
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
    if (password?.trim() !== '') {
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
  }, [basePasswordSchema, baseSchema, password, passwordSchema])

  const [removeImage, setRemoveImage] = useState(false)
  const [showPassword, setShowPassword] = useState(false)
  const [showPasswordConfirmation, setShowPasswordConfirmation] =
    useState(false)
  const [imageFile, setImageFile] = useState<File | null>(null)
  const [imgSrc, setImgSrc] = useState('')

  const { jwt, user } = useAuth()
  const [updateProfile, { loading }] = useMutation(UPDATE_PROFILE, {
    fetchPolicy: 'no-cache',
  })

  const dispatch = useAppDispatch()

  const defaultValues = useMemo(
    () => ({
      email: user?.email ?? '',
      password: '',
      passwordConfirmation: '',
    }),
    [user]
  )

  const onSubmit = async (form: FormData) => {
    const variables: UpdateProfileInput = {
      data: {
        email: form.email,
        password: form.password ?? null,
        imageFile,
        removeImage,
      },
    }

    if (!jwt?.sa) delete variables.data.email

    await updateProfile({ variables })
      .then(({ data }) => {
        if (data?.updateProfile) {
          toast.success(t('profileUpdatedSuccess'))
          dispatch(setUser(data.updateProfile))
        } else {
          toast.error(t('profileUpdatedError'))
        }
      })
      .catch((error) => toast.error(error?.message ?? t('profileUpdatedError')))
  }

  const {
    control,
    handleSubmit,
    formState: { errors },
  } = useForm({
    defaultValues,
    mode: 'onBlur',
    resolver: yupResolver(schema),
  })

  const handleInputImageChange = (file: ChangeEvent) => {
    const reader = new FileReader()
    const { files } = file.target as HTMLInputElement
    if (files && files.length !== 0) {
      reader.onload = () => setImgSrc(reader.result as string)
      reader.readAsDataURL(files[0])
      setImageFile(files[0])
    }
  }

  const handleInputFileClick = () => {
    document.getElementById('imageFile')?.click()
  }

  const handleRemoveImageClick = () => {
    Sawl.fire({
      title: t('confirmation'),
      text: t('removeProfilePhotoConfirm'),
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: t('yes'),
      cancelButtonText: t('no'),
    }).then(({ isConfirmed }) => {
      if (isConfirmed) {
        setImgSrc('')
        setImageFile(null)
        setRemoveImage(true)
        toast.success(t('removeProfilePhotoSuccess'))
      }
    })
  }

  useEffect(() => {
    if (user?.image) setImgSrc(user?.image ?? '')
  }, [user?.image])

  return (
    <Col xl={4}>
      <Card>
        <CommonCardHeading
          Heading={t('myProfile')}
          bigHeadingClassName="card-title mb-0"
        />
        <CardBody>
          <form
            className="user-profile"
            noValidate
            onSubmit={handleSubmit(onSubmit)}
          >
            <Row className="mb-5">
              <div className="col-auto">
                <div className="hovercard">
                  <div className="user-image">
                    <div className="avatar">
                      <Avatar
                        image={imgSrc ?? null}
                        name={user?.name ?? 'User'}
                        size={70}
                        className="img-70 rounded-circle"
                      />
                    </div>
                    <input
                      hidden
                      type="file"
                      accept="image/png, image/jpeg"
                      onChange={handleInputImageChange}
                      id="imageFile"
                    />
                    <div
                      id="removeFile"
                      className="icon-wrapper remove"
                      onClick={handleRemoveImageClick}
                    >
                      <i
                        className="icofont icofont-trash step2"
                        data-intro="Remove profile image here"
                      ></i>
                    </div>
                    <Tooltip
                      autohide
                      flip
                      isOpen={tooltipOpen1}
                      target="removeFile"
                      toggle={toggle1}
                    >
                      {t('removeProfilePhoto')}
                    </Tooltip>
                    <div
                      id="changeFile"
                      className="icon-wrapper update"
                      onClick={handleInputFileClick}
                    >
                      <i className="icofont icofont-pencil-alt-5 step2"></i>
                    </div>
                    <Tooltip
                      autohide
                      flip
                      isOpen={tooltipOpen2}
                      target="changeFile"
                      toggle={toggle2}
                    >
                      {t('changeProfilePhoto')}
                    </Tooltip>
                  </div>
                </div>
              </div>
              <Col>
                <h5 className="mb-1">{user?.name}</h5>
                <p className="mb-4">
                  {user?.roles?.length
                    ? user?.roles?.map((role) => role.name)?.join(', ')
                    : t('user')}
                </p>
              </Col>
            </Row>
            <FormGroup>
              <Label for="email" className={`fw-bold required`}>
                {t('email')}
              </Label>
              <Controller
                name="email"
                control={control}
                disabled={loading}
                rules={{ required: true }}
                render={({ field: { name, ...rest } }) => (
                  <Input
                    id={name}
                    type="email"
                    autoComplete="off"
                    placeholder={t('emailPlaceholder')}
                    invalid={Boolean(errors.email)}
                    readOnly={!jwt?.sa}
                    className={!jwt?.sa ? 'disabled' : ''}
                    {...rest}
                  />
                )}
              />
              <FormFeedback>
                {errors.email && errors.email.message}
              </FormFeedback>
            </FormGroup>
            <FormGroup>
              <Label
                for="password"
                className={`fw-bold ${password ? 'required' : ''}`}
              >
                {t('password')}
              </Label>
              <div className="form-input position-relative">
                <Controller
                  name="password"
                  control={control}
                  disabled={loading}
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
                <FormFeedback>
                  {errors.password && errors.password.message}
                </FormFeedback>
                <div className="show-hide">
                  <span
                    onClick={() => setShowPassword(!showPassword)}
                    className={!showPassword ? 'show' : ''}
                  >
                    {showPassword ? t('hide') : t('show')}
                  </span>
                </div>
              </div>
            </FormGroup>
            <FormGroup>
              <Label
                for="passwordConfirmation"
                className={`fw-bold ${password ? 'required' : ''}`}
              >
                {t('passwordConfirmation')}
              </Label>
              <div className="form-input position-relative">
                <Controller
                  name="passwordConfirmation"
                  control={control}
                  disabled={loading}
                  rules={{ required: true }}
                  render={({ field: { name, ...rest } }) => (
                    <Input
                      id={name}
                      autoComplete="off"
                      type={showPasswordConfirmation ? 'text' : 'password'}
                      placeholder=""
                      invalid={Boolean(errors.password)}
                      {...rest}
                    />
                  )}
                />
                <FormFeedback>
                  {errors.passwordConfirmation &&
                    errors.passwordConfirmation.message}
                </FormFeedback>
                <div className="show-hide">
                  <span
                    onClick={() =>
                      setShowPasswordConfirmation(!showPasswordConfirmation)
                    }
                    className={!showPasswordConfirmation ? 'show' : ''}
                  >
                    {showPasswordConfirmation ? t('hide') : t('show')}
                  </span>
                </div>
              </div>
            </FormGroup>
            <div className="form-footer">
              <Button
                color="primary"
                disabled={loading}
                className="d-block w-100"
                type="submit"
              >
                {t('save')}
              </Button>
            </div>
          </form>
        </CardBody>
      </Card>
    </Col>
  )
}

export default EditMyProfile
