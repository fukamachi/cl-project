(uiop:define-package <% @var name %>
  <%- @if use %>
  (:use<%(format t "~{ #:~(~A~)~^~}" (getf env :use)) %>)
  <%- @endif %>
  <%- @if import-from %>
  <%=(with-output-to-string (s)
       (let ((first-line t))
	 (mapc #'(lambda (import-from-items)
		   (if first-line
		       (setf first-line nil)
		       (format s "~%  "))
		   (if (consp import-from-items)
		       (format s "(:import-from~{ #:~(~A~)~^~%               ~})"
			       import-from-items)
		       (format s "(:import-from #:~(~A~))"
			       import-from-items)))
	       (getf env :import-from)))) %>
  <%- @endif %>
  <%- @if export %>
  (:export <% (format t "~{#:~(~A~)~^~%           ~}"
		      (getf env :export)) %>)
  <%- @endif %>)
(in-package #:<% @var name %>)

<% @var code %>
