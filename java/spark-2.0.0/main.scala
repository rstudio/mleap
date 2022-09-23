package mleap

import org.apache.spark.ml.bundle.SparkBundleContext
import ml.combust.mleap.spark.SparkSupport._
import org.apache.spark.ml.mleap.SparkUtil
import org.apache.spark.ml.Transformer
import ml.combust.bundle.BundleFile
import scala.language.postfixOps
import org.apache.spark.sql._
import org.apache.spark.ml._
import java.io.File
import resource._

object Main {
  def exportArrayToBundle(dataset: DataFrame, path: String, transformers: Transformer*) : Unit = {
    val pipeline = SparkUtil.createPipelineModel(transformers.toArray)
    implicit val sbc = SparkBundleContext().withDataset(dataset)
    for(bf <- managed(BundleFile("jar:" + path))) {
        pipeline.writeBundle.save(bf)get
      }
  }
  def importZipTransormer(path: String) : Transformer = {
    
    val zipBundle = (for(bundle <- managed(BundleFile("jar:" + path))) yield {
      bundle.loadSparkBundle().get
    }).opt.get
    
    zipBundle.root
  }
}
