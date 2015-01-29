package edu.colorado.walautil.cg;

import com.ibm.wala.ipa.callgraph.AnalysisCache;
import com.ibm.wala.ipa.callgraph.AnalysisOptions;
import com.ibm.wala.ipa.callgraph.CallGraph;
import com.ibm.wala.ipa.callgraph.ContextSelector;
import com.ibm.wala.ipa.callgraph.propagation.*;
import com.ibm.wala.ipa.callgraph.propagation.cfa.ZeroXContainerCFABuilder;
import com.ibm.wala.ipa.callgraph.propagation.cfa.ZeroXInstanceKeys;
import com.ibm.wala.ipa.cha.IClassHierarchy;
import com.ibm.wala.util.debug.Assertions;
import com.ibm.wala.util.intset.MutableMapping;
import com.ibm.wala.util.intset.OrdinalSet;

/** variant of ZeroXContainerCFABuilder with a better context selector policy and less of a memory footprint */
public class ImprovedZeroXContainerCFABuilder extends ZeroXContainerCFABuilder {

  public ImprovedZeroXContainerCFABuilder(IClassHierarchy cha, AnalysisOptions options, AnalysisCache cache,
                                          ContextSelector appContextSelector,
                                          SSAContextInterpreter appContextInterpreter, int instancePolicy) {

    super(cha, options, cache, appContextSelector, appContextInterpreter, instancePolicy);
  }

  @Override
  protected ContextSelector makeContainerContextSelector(IClassHierarchy cha, ZeroXInstanceKeys keys) {
    return new ImprovedContainerContextSelector(cha, keys);
  }

  static final class PropSystem extends PropagationSystem {
    private final PointerKeyFactory pointerKeyFactory;
    private final InstanceKeyFactory instanceKeyFactory;

    public PropSystem(CallGraph cg, PointerKeyFactory pointerKeyFactory, InstanceKeyFactory instanceKeyFactory) {
      super(cg, pointerKeyFactory, instanceKeyFactory);
      this.pointerKeyFactory = pointerKeyFactory;
      this.instanceKeyFactory = instanceKeyFactory;
    }

    @Override
    public PointerAnalysis<InstanceKey> makePointerAnalysis(PropagationCallGraphBuilder builder) {
      return new PointerAnalysisI(cg, pointsToMap, instanceKeys, pointerKeyFactory, instanceKeyFactory);
    }

  }

  // variant of PointerAnalysisImpl thtat does not retain a pointer to its CFA builder
  static final class PointerAnalysisI extends PointerAnalysisImpl {
    private final  PointsToMap pointsToMap;
    private final IClassHierarchy cha;

    public PointerAnalysisI(CallGraph cg, PointsToMap pointsToMap, MutableMapping<InstanceKey> instanceKeys,
                            PointerKeyFactory pointerKeys, InstanceKeyFactory iKeyFactory) {
      super(null, cg, pointsToMap, instanceKeys, pointerKeys, iKeyFactory);
      this.pointsToMap = pointsToMap;
      this.cha = cg.getClassHierarchy();
    }

    @Override
    // the original implementation of this method uses builder
    public IClassHierarchy getClassHierarchy() {
      return cha;
    }

    @Override
    public OrdinalSet<InstanceKey> getPointsToSet(PointerKey key) {
      if (pointsToMap.isImplicit(key)) {
        // TODO: could fix this case if we wanted to--this is the case that uses builder in the original implementation
        Assertions.UNREACHABLE();
      }
      return super.getPointsToSet(key);
    }
  }



  @Override
  protected PropagationSystem makeSystem(AnalysisOptions options) {
    return new PropSystem(callGraph, pointerKeyFactory, instanceKeyFactory);
  }

}
