"use client";

import { useMutation } from "@apollo/client";
import { yupResolver } from "@hookform/resolvers/yup";
import { useTranslations } from "next-intl";
import { useState } from "react";
import { Controller, useForm } from "react-hook-form";
import { FaFacebook, FaGoogle } from "react-icons/fa";
import { toast } from "react-toastify";
import {
  Button,
  Col,
  FormFeedback,
  FormGroup,
  Input,
  Label,
  Modal,
  ModalBody,
  ModalFooter,
  ModalHeader,
  Row,
} from "reactstrap";
import * as yup from "yup";

import { EMAIL_REGEX, PASSWORD_STRENGTH_REGEX } from "@/configs";
import { IS_DEVELOPMENT, SERVER_URL } from "@/environment";
import { LOGIN } from "@/graphql/auth";
import { useAppDispatch } from "@/hooks";
import { Link } from "@/navigation";
import { setAuth } from "@/store/slices/authSlice";
import { LoginInput } from "@/types/auth";

type LoginProps = {
  isOpen: boolean;
  toggle: () => void;
  setLoggedIn: (value: boolean) => void;
};

type FormData = {
  email: string;
  password: string;
};

const defaultValues = IS_DEVELOPMENT
  ? {
      email: "amandasmith@me.com",
      password: "abClK1@X",
    }
  : {
      email: "",
      password: "",
    };

export const LoginModal = ({ isOpen, toggle, setLoggedIn }: LoginProps) => {
  const [disabled, setDisabled] = useState(false);
  const [rememberMe, setRememberMe] = useState(false);
  const [showPassword, setShowPassword] = useState(false);
  const [login] = useMutation(LOGIN, { fetchPolicy: "no-cache" });

  const t = useTranslations();
  const dispatch = useAppDispatch();

  const schema = yup.object().shape({
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

  const onSubmit = async (form: FormData) => {
    setDisabled(true);

    const variables: LoginInput = {
      data: { email: form.email, password: form.password, rememberMe },
    };

    await login({ variables })
      .then(({ data }) => {
        if (data?.login) {
          toast.success(t("loginSuccess"));
          dispatch(setAuth(data.login));
          setLoggedIn(true);
        } else {
          toast.error(t("loginError"));
          setLoggedIn(false);
        }
      })
      .catch((error) => {
        toast.error(error?.message ?? t("loginError"));
        setLoggedIn(true);
      });
    toggle();
    setDisabled(false);
  };

  return (
    <Modal keyboard={false} isOpen={isOpen} toggle={toggle}>
      <form
        noValidate
        autoComplete="off"
        onSubmit={handleSubmit(onSubmit)}
        className="theme-form"
      >
        <ModalHeader toggle={toggle}>{t("signInToAccount")}</ModalHeader>
        <ModalBody>
          <div className="login-modal">
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
                    autoFocus
                    autoComplete="on"
                    placeholder={t("emailPlaceholder")}
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
              <Label htmlFor="password" className="col-form-label">
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
            </FormGroup>
            <FormGroup className="mb-0 form-group">
              <Row>
                <Col md="6" xs="12">
                  <div className="checkbox p-0">
                    <Input
                      id="rememberMe"
                      type="checkbox"
                      disabled={disabled}
                      onChange={() => setRememberMe(!rememberMe)}
                    />
                    <Label className="text-muted" htmlFor="rememberMe">
                      {t("rememberMe")}
                    </Label>
                  </div>
                </Col>
                <Col md="6" xs="12">
                  <div className="text-md-end mt-0">
                    <Link
                      className={disabled ? "disabled" : ""}
                      href={disabled ? "#" : "/auth/forgot-password"}
                    >
                      {t("forgotYourPassword")}
                    </Link>
                  </div>
                </Col>
              </Row>
            </FormGroup>
            <h6 className="text-muted mt-4 or">{t("orSignInWith")}</h6>
            <div className="social mt-4">
              <div className="btn-showcase">
                <Link
                  className={`btn btn-light ${disabled ? "disabled" : ""}`}
                  href={disabled ? "#" : `${SERVER_URL}/auth/facebook`}
                  rel="noreferrer"
                  title={t("facebook")}
                >
                  <FaFacebook className="social-icon txt-facebook" />
                  {t("facebook")}
                </Link>
                <Link
                  className={`btn btn-light ${disabled ? "disabled" : ""}`}
                  href={disabled ? "#" : `${SERVER_URL}/auth/google`}
                  rel="noreferrer"
                  title={t("google")}
                >
                  <FaGoogle className="social-icon txt-google" />
                  {t("google")}
                </Link>
              </div>
            </div>
            <p className="mt-3 py-3 mb-0 text-center">
              {t("dontHaveAnAccount")}
              <Link
                className={`ms-2 ${disabled ? "disabled" : ""}`}
                href={disabled ? "#" : "/auth/register"}
              >
                {t("createAccount")}
              </Link>
            </p>
          </div>
        </ModalBody>
        <ModalFooter>
          <Button type="submit" disabled={disabled} color="primary">
            {t("signIn")}
          </Button>
          <Button disabled={disabled} onClick={toggle}>
            {t("close")}
          </Button>
        </ModalFooter>
      </form>
    </Modal>
  );
};
