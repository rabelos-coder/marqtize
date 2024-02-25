"use client";

import { Col, Container, Row } from "reactstrap";
import Link from "next/link";
import { ChangeEvent, FormEvent, useState } from "react";
import { Facebook, Linkedin, Twitter } from "react-feather";
import { Button, FormGroup, Input, Label } from "reactstrap";
import { toast } from "react-toastify";
import { useRouter } from "@/navigation";
import { CommonLogo } from "@/components/common/CommonLogo";
import { signIn } from "next-auth/react";

export const Login = () => {
  const [rememberMe, setRememberMe] = useState(false);
  const [showPassWord, setShowPassWord] = useState(false);
  const [formValues, setFormValues] = useState({
    email: "Test@gmail.com",
    password: "Test@123",
  });
  const { email, password } = formValues;
  const router = useRouter();
  const handleUserValue = (event: ChangeEvent<HTMLInputElement>) => {
    setFormValues({ ...formValues, [event.target.name]: event.target.value });
  };
  const formSubmitHandle = (event: FormEvent) => {
    event.preventDefault();
    signIn("credentials", {
      email,
      password,
      rememberMe,
      redirect: false,
    }).then((res) => {
      if (res && res.ok) {
        router.replace("/backend");
      } else {
        toast.error("Email or password is invalid");
      }
    });
  };

  return (
    <Container fluid className="p-0">
      <Row className="m-0">
        <Col xs={12} className="p-0">
          <div className="login-card login-dark">
            <div>
              <div>
                <CommonLogo />
              </div>
              <div className="login-main">
                <form className="theme-form" onSubmit={formSubmitHandle}>
                  <h4>Sign in to account</h4>
                  <p>Enter your email & password to login</p>
                  <FormGroup>
                    <Label className="col-form-label">Email</Label>
                    <Input
                      type="email"
                      required
                      placeholder="Test@gmail.com"
                      value={email}
                      name="email"
                      onChange={handleUserValue}
                    />
                  </FormGroup>
                  <FormGroup>
                    <Label className="col-form-label">Password</Label>
                    <div className="form-input position-relative">
                      <Input
                        type={showPassWord ? "text" : "password"}
                        placeholder="*********"
                        onChange={handleUserValue}
                        value={password}
                        name="password"
                      />
                      <div className="show-hide">
                        <span
                          onClick={() => setShowPassWord(!showPassWord)}
                          className={!showPassWord ? "show" : ""}
                        />
                      </div>
                    </div>
                  </FormGroup>
                  <FormGroup className="mb-0 form-group">
                    <div className="checkbox p-0">
                      <Input
                        id="rememberMe"
                        type="checkbox"
                        onChange={() => setRememberMe(!rememberMe)}
                      />
                      <Label className="text-muted" htmlFor="rememberMe">
                        Remember me
                      </Label>
                    </div>
                    <Link
                      className="link"
                      href="/pages/authentication/forget-pwd"
                    >
                      Forgot password?
                    </Link>
                    <div className="text-end mt-3">
                      <Button
                        color="primary"
                        className="btn-block w-100"
                        type="submit"
                      >
                        Sign in
                      </Button>
                    </div>
                  </FormGroup>
                  <h6 className="text-muted mt-4 or">Or Sign in with</h6>
                  <div className="social mt-4">
                    <div className="btn-showcase">
                      <a
                        className="btn btn-light"
                        href="https://www.linkedin.com/login"
                        target="_blank"
                        rel="noreferrer"
                      >
                        {" "}
                        <Linkedin className="txt-linkedin" /> LinkedIn
                      </a>
                      <a
                        className="btn btn-light"
                        href="https://twitter.com/login?lang=en"
                        target="_blank"
                        rel="noreferrer"
                      >
                        <Twitter className="txt-twitter" />
                        Twitter
                      </a>
                      <a
                        className="btn btn-light"
                        href="https://www.facebook.com/"
                        target="_blank"
                        rel="noreferrer"
                      >
                        <Facebook className="txt-fb" />
                        Facebook
                      </a>
                    </div>
                  </div>
                  <p className="mt-4 mb-0 text-center">
                    Don&apos;t have account?
                    <Link
                      className="ms-2"
                      href="/pages/authentication/register-simple"
                    >
                      Create Account
                    </Link>
                  </p>
                </form>
              </div>
            </div>
          </div>
        </Col>
      </Row>
    </Container>
  );
};
