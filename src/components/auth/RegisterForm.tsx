"use client";

import { useMutation } from "@apollo/client";
import { yupResolver } from "@hookform/resolvers/yup";
import { useTranslations } from "next-intl";
import { useEffect, useState } from "react";
import { Controller, useForm } from "react-hook-form";
import { toast } from "react-toastify";
import {
  Button,
  Col,
  FormFeedback,
  FormGroup,
  Input,
  Label,
  Row,
} from "reactstrap";
import * as yup from "yup";

import { CommonLogo } from "@/components/common/CommonLogo";
import { EMAIL_REGEX, PASSWORD_STRENGTH_REGEX } from "@/configs";
import { IS_DEVELOPMENT } from "@/environment";
import { REGISTER } from "@/graphql/auth";
import { useAppDispatch, useAppSelector } from "@/hooks";
import { Link, useRouter } from "@/navigation";
import { resetError } from "@/store/slices/authSlice";
import { fetchCustomer } from "@/store/slices/customerSlice";

import { SpinnerBoxed } from "../common/SpinnerBoxed";

type RegisterProps = {
  host: string;
  alignLogo?: string;
};

type FormData = {
  name: string;
  systemName: string;
  email: string;
  password: string;
  passwordConfirmation: string;
};

const defaultValues = IS_DEVELOPMENT
  ? {
      name: "Amanda Smith",
      systemName: "Amanda",
      email: "amandasmith@me.com",
      password: "abClK1@X",
      passwordConfirmation: "abClK1@X",
    }
  : {
      name: "",
      systemName: "",
      email: "",
      password: "",
      passwordConfirmation: "",
    };

export const RegisterForm = ({ host, alignLogo }: RegisterProps) => {
  const [showPassword, setShowPassword] = useState(false);
  const [showPasswordConfirmation, setShowPasswordConfirmation] =
    useState(false);
  const [register, { data, loading, error }] = useMutation(REGISTER);

  const t = useTranslations("translations");
  const dispatch = useAppDispatch();
  const { customer, loading: customerLoading } = useAppSelector(
    (state) => state.customer
  );

  const schema = yup.object().shape({
    name: yup.string().required(t("invalidNameRequired")),
    systemName: yup.string().required(t("invalidSystemNameRequired")),
    email: yup
      .string()
      .email(t("invalidEmailFormat"))
      .required(t("invalidEmailRequired"))
      .matches(new RegExp(EMAIL_REGEX), t("invalidEmailFormat")),
    password: yup
      .string()
      .trim()
      .required(t("invalidPasswordRequired"))
      .matches(
        new RegExp(PASSWORD_STRENGTH_REGEX),
        t("invalidPasswordStrength")
      ),
    passwordConfirmation: yup
      .string()
      .trim()
      .required(t("invalidPasswordConfirmationRequired"))
      .matches(
        new RegExp(PASSWORD_STRENGTH_REGEX),
        t("invalidPasswordStrength")
      )
      .oneOf(
        [yup.ref("password"), ""],
        t("invalidPasswordConfirmationMustMatch")
      ),
  });

  const {
    control,
    handleSubmit,
    formState: { errors },
  } = useForm({
    defaultValues,
    mode: "onBlur",
    resolver: yupResolver(schema),
  });

  const router = useRouter();

  const onSubmit = async (form: FormData) => {
    await register({
      variables: {
        data: {
          customerId: customer?.id ?? null,
          name: form.name,
          systemName: form.systemName,
          email: form.email,
          password: form.password,
        },
      },
    });
    if (data?.register) {
      toast.success(t("resetPasswordSuccess"));
      router.push("/auth/login");
    } else {
      toast.error(t("resetPasswordError"));
    }
  };

  useEffect(() => {
    dispatch(resetError());
    dispatch(fetchCustomer(host));
    if (error) toast.error(error.message);
  }, [error, dispatch, host]);

  return customerLoading ? (
    <SpinnerBoxed type="grow" />
  ) : (
    <div className="login-card login-dark">
      <div>
        <div>
          <CommonLogo alignLogo={alignLogo} />
        </div>
        <div className="login-main">
          <form
            className="theme-form"
            noValidate
            autoComplete="off"
            onSubmit={handleSubmit(onSubmit)}
          >
            <h4 suppressHydrationWarning>
              {customer?.tradingName
                ? t("createAccountToName", { name: customer.tradingName })
                : t("createAccount")}
            </h4>
            <p>{t("resetPasswordInfo")}</p>
            <FormGroup>
              <Row className="g-2">
                <Col xs={6}>
                  <Label htmlFor="name" className="col-form-label pt-0">
                    {t("name")}
                  </Label>
                  <Controller
                    name="name"
                    control={control}
                    rules={{ required: true }}
                    render={({
                      field: { name, value, onChange, onBlur, ...rest },
                    }) => (
                      <Input
                        id={name}
                        autoFocus
                        placeholder={t("namePlaceholder")}
                        value={value}
                        onBlur={onBlur}
                        onChange={onChange}
                        valid={!Boolean(errors.name)}
                        invalid={Boolean(errors.name)}
                        {...rest}
                      />
                    )}
                  />
                  <FormFeedback>
                    {errors.name && errors.name.message}
                  </FormFeedback>
                </Col>
                <Col xs={6}>
                  <Label htmlFor="systemName" className="col-form-label pt-0">
                    {t("systemName")}
                  </Label>
                  <Controller
                    name="systemName"
                    control={control}
                    rules={{ required: true }}
                    render={({
                      field: { name, value, onChange, onBlur, ...rest },
                    }) => (
                      <Input
                        id={name}
                        autoFocus
                        placeholder={t("systemNamePlaceholder")}
                        value={value}
                        onBlur={onBlur}
                        onChange={onChange}
                        valid={!Boolean(errors.systemName)}
                        invalid={Boolean(errors.systemName)}
                        {...rest}
                      />
                    )}
                  />
                  <FormFeedback>
                    {errors.systemName && errors.systemName.message}
                  </FormFeedback>
                </Col>
              </Row>
            </FormGroup>
            <FormGroup>
              <Label htmlFor="email" className="col-form-label">
                {t("email")}
              </Label>
              <Controller
                name="email"
                control={control}
                rules={{ required: true }}
                render={({
                  field: { name, value, onChange, onBlur, ...rest },
                }) => (
                  <Input
                    id={name}
                    type="email"
                    autoFocus
                    placeholder={t("emailPlaceholder")}
                    value={value}
                    onBlur={onBlur}
                    onChange={onChange}
                    valid={!Boolean(errors.email)}
                    invalid={Boolean(errors.email)}
                    {...rest}
                  />
                )}
              />
              <FormFeedback>
                {errors.email && errors.email.message}
              </FormFeedback>
            </FormGroup>
            <FormGroup>
              <Row className="g-2">
                <Col xs={6}>
                  <Label htmlFor="password" className="col-form-label pt-0">
                    {t("password")}
                  </Label>
                  <div className="form-input position-relative">
                    <Controller
                      name="password"
                      control={control}
                      rules={{ required: true }}
                      render={({
                        field: { name, value, onChange, onBlur, ...rest },
                      }) => (
                        <Input
                          id={name}
                          type={showPassword ? "text" : "password"}
                          placeholder="*********"
                          value={value}
                          onBlur={onBlur}
                          onChange={onChange}
                          valid={!Boolean(errors.password)}
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
                        className={!showPassword ? "show" : ""}
                      >
                        {showPassword ? t("hide") : t("show")}
                      </span>
                    </div>
                  </div>
                </Col>
                <Col xs={6}>
                  <Label
                    htmlFor="passwordConfirmation"
                    className="col-form-label pt-0"
                  >
                    {t("passwordConfirmation")}
                  </Label>
                  <div className="form-input position-relative">
                    <Controller
                      name="passwordConfirmation"
                      control={control}
                      rules={{ required: true }}
                      render={({
                        field: { name, value, onChange, onBlur, ...rest },
                      }) => (
                        <Input
                          id={name}
                          type={showPasswordConfirmation ? "text" : "password"}
                          placeholder="*********"
                          value={value}
                          onBlur={onBlur}
                          onChange={onChange}
                          valid={!Boolean(errors.passwordConfirmation)}
                          invalid={Boolean(errors.passwordConfirmation)}
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
                        className={!showPasswordConfirmation ? "show" : ""}
                      >
                        {showPasswordConfirmation ? t("hide") : t("show")}
                      </span>
                    </div>
                  </div>
                </Col>
              </Row>
            </FormGroup>
            <FormGroup className="mb-0 form-group">
              <div className="text-end mt-3">
                <Button
                  color="primary"
                  className="btn-block w-100"
                  type="submit"
                  disabled={loading}
                >
                  {t("register")}
                </Button>
              </div>
              <p className="mt-4 mb-0 text-center">
                {t("alreadyHaveAnAccount")}
                <Link className="ms-2" href="/auth/login">
                  {t("signIn")}
                </Link>
              </p>
            </FormGroup>
          </form>
        </div>
      </div>
    </div>
  );
};
