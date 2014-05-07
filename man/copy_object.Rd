\name{copy_object}
\alias{copy_object}
\title{Copy any object}
\usage{
copy_object(object)
}
\arguments{
  \item{object}{object to be copied}
}
\description{
Copy any object with all environments. Saves the object and
loads it again in an empty environment. These steps are
neccessary to copy all environments and environment
bindings.
}

