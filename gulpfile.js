const gulp = require('gulp')
const sourcemaps = require('gulp-sourcemaps')
const sass = require('gulp-sass')(require('sass'))
const path = require('path')

const scssFolder = 'src/assets/scss'
const scssSource = `${scssFolder}/**/**.scss`
const cssFolder = 'src/assets/css'

function compileSass() {
  return gulp
    .src(scssSource)
    .pipe(sourcemaps.init({ loadMaps: true }))
    .pipe(
      sass({
        outputStyle: 'expanded',
        sourceMap: true,
        includePaths: [path.join(process.cwd(), scssFolder)],
      }).on('error', sass.logError)
    )
    .pipe(
      sourcemaps.write('.', {
        sourceRoot: scssFolder,
      })
    )
    .pipe(gulp.dest(cssFolder))
}

function watchSass() {
  return gulp.watch(scssSource, compileSass)
}

gulp.task('sass', compileSass)
gulp.task('watch', watchSass)
