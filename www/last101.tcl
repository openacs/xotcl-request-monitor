ad_page_contract {
    Displays last 100 requests in the system

    @author Gustaf Neumann

    @cvs-id $Id$
} -query {
    {orderby:token,optional "time,desc"}
} -properties {
    title:onevalue
    context:onevalue
}

set title "Last 100 Requests"
set context [list "Last 100 Requests"]
set stat [list]
foreach {key value} [throttle last100] {lappend stat $value}

Class create CustomField -volatile \
    -instproc render-data {row} {
      html::div -style {
        border: 1px solid #a1a5a9; padding: 0px 5px 0px 5px; background: #e2e2e2} {
          html::t  [$row set ${:name}]
        }
    }

TableWidget create t1 -volatile \
    -columns {
      Field time       -label "Time" -orderby time -mixin ::template::CustomField
      AnchorField user -label "Userid" -orderby user
      Field ms         -label "Ms" -orderby ms
      Field url        -label "URL" -orderby url
    }

lassign [split $orderby ,] att order
t1 orderby \
    -order [ad_decode $order desc decreasing asc increasing increasing] \
    -type [ad_decode $att ms integer dictionary] \
    $att

foreach l $stat {
  lassign $l timestamp c url ms requester
  set user_info   [xo::request_monitor_user_info $requester]
  set user_string [dict get $user_info label]

  t1 add -time [clock format $timestamp -format "%H:%M:%S"] \
      -user $user_string \
      -user.href [export_vars -base last-requests {{request_key $requester}}] \
      -ms $ms \
      -url $url
}



Object instproc asHTML {{-master defaultMaster} -page:switch} {
  ::xo::require_html_procs
  dom createDocument html doc
  set root [$doc documentElement]
  if {!$page} {
    $root appendFromScript {:render}
    return [[$root childNode] asHTML]
  } else {
    set slave [$master decorate $root]
    $slave appendFromScript {:render}
    ns_return 200 text/html [$root asHTML]
  }
}

Object ::pageMaster -proc decorate {node} {
  $node appendFromScript {
    html::head {
      html::title {html::t "XOTcl Request Monitor"}
      html::link -rel stylesheet -type text/css -media all -href \
          /resources/acs-developer-support/acs-developer-support.css
      html::link -rel stylesheet -type text/css -media all -href \
          /resources/acs-templating/lists.css
      html::link -rel stylesheet -type text/css -media all -href \
          /resources/acs-templating/forms.css
      html::link -rel stylesheet -type text/css -media all -href \
          /resources/acs-subsite/default-master.css
      html::link -rel stylesheet -type text/css -media all -href \
          /resources/dotlrn/dotlrn-toolbar.css
      html::script -type "text/javascript" -src "/resources/acs-subsite/core.js" \
          -language "javascript" {}
      html::link -rel "shortcut icon" \
          -href "/resources/theme-selva/Selva/default/images/myicon.ico"
      html::link -rel "stylesheet" -type "text/css" \
          -href "/resources/theme-selva/Selva/default/Selva.css" -media "all"
    }
    html::body {
      html::div -id wrapper {
        html::div -id header {
          html::img -src /resources/theme-selva/Selva/images/dotLRN-logo.gif \
              -alt Logo
        }
        html::br
        html::div -id site-header {
          html::div -id breadcrumbs {
            html::div -id context-bar {
              html::a -href / {html::t "Main Site"}
              html::t -disableOutputEscaping "&#187;\n"
              html::a -href "/request-monitor" {html::t "XOTcl Request Monitor"}
              html::t -disableOutputEscaping "&#187;\n"
              html::t ${:context}
              html::div -style "clear:both;"
            }
            html::div -id status {
              html::div -class "action-list users-online" {
                html::a -href "/shared/whos-online" {
                  html::t "1 member online"
                }
                html::t "|"
                html::a -href "/register/logout" \
                    -title "Von yourdomain Network abmelden" {
                      html::t Abmelden
                    }
              }
              html::div -class "user-greeting" {
                html::t "Willkommen, Gustaf Neumann!  |"
              }
            }
          }
        } ;# end of site header
        html::div -id "youarehere" {
          html::t ${:title}
        }
        html::br
        html::div -id "portal-navigation" {
          html::ul {
            html::li {html::a -href "/dotlrn/" {html::t "My Space"}}
            html::li {html::a -href "/theme-selva/courses" {html::t "Courses"}}
            html::li {html::a -href "/theme-selva/communities" {html::t "Communities"}}
            html::li {html::a -href "/pvt/home" {html::t "Einstellungen"}}
            html::li {html::a -href "/dotlrn/control-panel" {html::t "Tools"}}
          }
        }

        html::div -id "portal-subnavigation" {
          html::div -id "portal-subnavigation-links" {
            html::ul {
              html::li {
                html::a -href "/dotlrn/?page_num=0" {html::t "Eigene Startseite"}
              }
              html::li {
                html::a -href "/dotlrn/?page_num=1" {html::t "Eigener Kalender"}
              }
              html::li {
                html::a -href "/dotlrn/?page_num=2" {html::t "Eigene Dateien"}
              }
            }
          }
        }

        html::div -id "portal" {
          set slave [tmpl::div]
        }
      }
    }
    html::t "hello footer"
  }
  return $slave
}
pageMaster set title $title
pageMaster set context [lindex $context 0]

ns_log notice "render time [time {t1 asHTML -page -master ::pageMaster}]"
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
