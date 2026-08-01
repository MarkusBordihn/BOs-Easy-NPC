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

package de.markusbordihn.easynpc.client.pose;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.animation.AnimationData;
import de.markusbordihn.easynpc.data.animation.AnimationData.Animation;
import de.markusbordihn.easynpc.data.animation.AnimationData.Bone;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.util.LinkedHashMap;
import java.util.Map;
import net.minecraft.resources.Identifier;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class PoseManagerTest {

  private static AnimationData animationData(String animationName) {
    Animation animation = new Animation();
    animation.setName(animationName);
    Map<String, Bone> bones = new LinkedHashMap<>();
    bones.put("head", new Bone());
    animation.setBones(bones);

    Map<String, Animation> animations = new LinkedHashMap<>();
    animations.put(animationName, animation);
    AnimationData animationData = new AnimationData();
    animationData.setAnimations(animations);
    return animationData;
  }

  private static Identifier poseLocation(String path) {
    return Identifier.fromNamespaceAndPath(Constants.MOD_ID, "pose/humanoid/" + path);
  }

  @BeforeEach
  @AfterEach
  void clearPoseData() {
    PoseManager.clearPoseData();
  }

  @Test
  @DisplayName("Pose names with special characters are registered as readable ids")
  void registersReadableIds() {
    PoseManager.registerPoseData(SkinModel.HUMANOID, animationData("Sitzen Ä"));

    assertNotNull(PoseManager.getPoseData(poseLocation("sitzen_ae")));
  }

  @Test
  @DisplayName("Poses stored with the previous id are still found")
  void findsPosesStoredWithTheLegacyId() {
    PoseManager.registerPoseData(SkinModel.HUMANOID, animationData("Sit Down"));

    assertNotNull(PoseManager.getPoseData(poseLocation("sit_down")));
    assertNotNull(PoseManager.getPoseData(poseLocation("sitdown")));
    assertEquals(
        PoseManager.getPoseData(poseLocation("sit_down")),
        PoseManager.getPoseData(poseLocation("sitdown")));
    assertEquals(1, PoseManager.getPoseDataKeysForModel(SkinModel.HUMANOID).size());
  }

  @Test
  @DisplayName("Unknown poses stay unknown")
  void unknownPosesStayUnknown() {
    PoseManager.registerPoseData(SkinModel.HUMANOID, animationData("Sit Down"));

    assertNull(PoseManager.getPoseData(poseLocation("stand_up")));
    assertNull(PoseManager.getPoseData(null));
  }
}
