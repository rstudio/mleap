package mleap

import org.apache.spark.ml.mleap.SparkUtil
import ml.combust.mleap.spark.SparkSupport._
import resource._
import ml.combust.bundle.BundleFile
import org.apache.spark.ml.bundle.SparkBundleContext

import org.apache.spark.ml.Transformer
import org.apache.spark.sql._

import java.io.File

object Main {
  def exportArrayToBundle(dataset: DataFrame, path: String, transformers: Transformer*) : Unit = {
    val pipeline = SparkUtil.createPipelineModel(transformers.toArray)
    implicit val sbc = SparkBundleContext().withDataset(dataset)
    for(bf <- managed(BundleFile("jar:" + path))) {
        pipeline.writeBundle.save(bf)get
      }
  }
}
