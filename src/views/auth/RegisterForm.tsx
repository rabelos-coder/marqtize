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
import { setLoading } from "@/store/slices/themeSlice";
import { AuthFormProps } from "@/types/common";

import { SpinnerBoxed } from "../../components/common/SpinnerBoxed";

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

export const RegisterForm = ({ alignLogo }: AuthFormProps) => {
  const [disabled, setDisabled] = useState(false);
  const [showPassword, setShowPassword] = useState(false);
  const [showPasswordConfirmation, setShowPasswordConfirmation] =
    useState(false);
  const [register] = useMutation(REGISTER);

  const t = useTranslations("translations");
  const dispatch = useAppDispatch();

  const { customer, loading } = useAppSelector((state) => state.customer);

  const schema = yup.object().shape({
    name: yup.string().required(t("propertyRequired", { property: t("name") })),
    systemName: yup
      .string()
      .required(t("propertyRequired", { property: t("systemName") })),
    email: yup
      .string()
      .email(t("propertyEmail", { property: t("email") }))
      .required(t("propertyRequired", { property: t("email") }))
      .matches(
        new RegExp(EMAIL_REGEX),
        t("propertyEmail", { property: t("email") })
      ),
    password: yup
      .string()
      .trim()
      .required(t("propertyRequired", { property: t("password") }))
      .matches(
        new RegExp(PASSWORD_STRENGTH_REGEX),
        t("propertyStrength", { property: t("password") })
      ),
    passwordConfirmation: yup
      .string()
      .trim()
      .required(t("propertyRequired", { property: t("passwordConfirmation") }))
      .oneOf(
        [yup.ref("password")],
        t("propertyMatch", {
          property: t("passwordConfirmation"),
          match: t("password"),
        })
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
    setDisabled(true);
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
    })
      .then(({ data }) => {
        if (data?.register) {
          toast.success(t("registerSuccess"));
          router.push("/auth/login");
        } else {
          toast.error(t("registerError"));
        }
      })
      .catch((error) => toast.error(error?.message ?? t("registerError")));

    setDisabled(false);
  };

  useEffect(() => {
    dispatch(setLoading(false));
  }, [dispatch]);

  return loading ? (
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
                    disabled={disabled}
                    rules={{ required: true }}
                    render={({ field: { name, ...rest } }) => (
                      <Input
                        id={name}
                        autoFocus
                        placeholder={t("namePlaceholder")}
                        autoComplete="on"
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
                    disabled={disabled}
                    rules={{ required: true }}
                    render={({ field: { name, ...rest } }) => (
                      <Input
                        id={name}
                        placeholder={t("systemNamePlaceholder")}
                        autoComplete="on"
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
                disabled={disabled}
                rules={{ required: true }}
                render={({ field: { name, ...rest } }) => (
                  <Input
                    id={name}
                    type="email"
                    placeholder={t("emailPlaceholder")}
                    autoComplete="on"
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
                      disabled={disabled}
                      rules={{ required: true }}
                      render={({ field: { name, ...rest } }) => (
                        <Input
                          id={name}
                          type={showPassword ? "text" : "password"}
                          autoComplete="off"
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
                      disabled={disabled}
                      rules={{ required: true }}
                      render={({ field: { name, ...rest } }) => (
                        <Input
                          id={name}
                          type={showPasswordConfirmation ? "text" : "password"}
                          autoComplete="off"
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
                  disabled={disabled}
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
