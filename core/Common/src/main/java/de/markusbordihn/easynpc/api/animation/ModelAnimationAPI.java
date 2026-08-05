/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.api.animation;

import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.data.model.ModelAnimationOperation;
import de.markusbordihn.easynpc.data.model.ModelAnimationPlaybackMode;
import de.markusbordihn.easynpc.data.model.ModelAnimationRequest;
import de.markusbordihn.easynpc.data.model.ModelAnimationTransition;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelAnimationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import java.util.List;
import java.util.Locale;
import java.util.Set;

public final class ModelAnimationAPI {

  public static final int MAX_ANIMATION_NAME_LENGTH = 256;
  private static final String NAMED_PREFIX = "named:";
  private static final Set<String> STANDARD_ANIMATIONS =
      Set.of("auto", "idle", "walk", "run", "hurt", "death", "swim", "fly", "attack");

  private ModelAnimationAPI() {}

  public static boolean supportsAnimations(EasyNPC<?> npc) {
    return CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED
        && npc instanceof EasyModelNPC
        && npc instanceof ModelAnimationDataCapable<?>;
  }

  public static boolean playAnimation(EasyNPC<?> npc, String animationName) {
    return playAnimation(
        npc, animationName, ModelAnimationPlaybackMode.ONCE, ModelAnimationTransition.DEFAULT);
  }

  public static boolean playAnimation(
      EasyNPC<?> npc,
      String animationName,
      ModelAnimationPlaybackMode playbackMode,
      ModelAnimationTransition transition) {
    String normalizedName = normalizeAnimationName(animationName);
    if (normalizedName.isEmpty()) {
      return false;
    }
    return issueRequest(
        npc, ModelAnimationOperation.PLAY, normalizedName, playbackMode, transition);
  }

  public static boolean stopAnimation(EasyNPC<?> npc) {
    return stopAnimation(npc, ModelAnimationTransition.DEFAULT);
  }

  public static boolean stopAnimation(EasyNPC<?> npc, ModelAnimationTransition transition) {
    return issueRequest(
        npc, ModelAnimationOperation.STOP, "", ModelAnimationPlaybackMode.ONCE, transition);
  }

  public static boolean restartAnimation(EasyNPC<?> npc) {
    return issueRequest(
        npc,
        ModelAnimationOperation.RESTART,
        "",
        ModelAnimationPlaybackMode.ONCE,
        ModelAnimationTransition.DEFAULT);
  }

  public static List<ModelAnimationInfo> listAnimations(EasyNPC<?> npc) {
    if (!(npc instanceof EasyModelNPC easyModelNPC) || !supportsAnimations(npc)) {
      return List.of();
    }
    return EasyModelEntitiesManager.listAnimations(easyModelNPC.getEasyModelProfileId());
  }

  public static String normalizeAnimationName(String animationName) {
    if (animationName == null) {
      return "";
    }
    String normalized = animationName.trim().toLowerCase(Locale.ROOT);
    if (normalized.isEmpty() || normalized.length() > MAX_ANIMATION_NAME_LENGTH) {
      return "";
    }
    if (normalized.startsWith(NAMED_PREFIX)) {
      return normalized.length() > NAMED_PREFIX.length() ? normalized : "";
    }
    return STANDARD_ANIMATIONS.contains(normalized) ? normalized : NAMED_PREFIX + normalized;
  }

  public static Set<String> standardAnimations() {
    return STANDARD_ANIMATIONS;
  }

  private static boolean issueRequest(
      EasyNPC<?> npc,
      ModelAnimationOperation operation,
      String animationName,
      ModelAnimationPlaybackMode playbackMode,
      ModelAnimationTransition transition) {
    if (!supportsAnimations(npc)
        || playbackMode == null
        || transition == null
        || !(npc instanceof ModelAnimationDataCapable<?> animationData)) {
      return false;
    }

    ModelAnimationRequest current = animationData.getModelAnimationRequest();
    int sequence = current.sequence() == Integer.MAX_VALUE ? 1 : current.sequence() + 1;
    animationData.setModelAnimationRequest(
        new ModelAnimationRequest(
            operation,
            animationName,
            playbackMode,
            transition,
            sequence,
            npc.getEntity().level().getGameTime()));
    return true;
  }
}
