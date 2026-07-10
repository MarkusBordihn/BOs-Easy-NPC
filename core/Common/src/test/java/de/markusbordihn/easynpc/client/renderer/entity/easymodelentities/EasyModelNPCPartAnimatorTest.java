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

package de.markusbordihn.easynpc.client.renderer.entity.easymodelentities;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import de.markusbordihn.easymodelentities.api.client.EasyModelPartAnimator;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelPartAnimationContext;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelPartTransform;
import de.markusbordihn.easymodelentities.data.profile.ModelBodyType;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import java.util.EnumMap;
import org.junit.jupiter.api.Test;

class EasyModelNPCPartAnimatorTest {

  private static EasyModelPartAnimationContext context(String partName) {
    return new EasyModelPartAnimationContext(
        partName, ModelBodyType.HUMANOID, 0.0f, 0.0f, 0.0f, 0.0f, EasyModelPartTransform.NONE);
  }

  @Test
  void returnsNoneForEmptySnapshot() {
    assertSame(
        EasyModelPartAnimator.NONE,
        EasyModelNPCPartAnimator.snapshot(new EnumMap<>(ModelPartType.class)));
  }

  @Test
  void mapsCanonicalBoneNameToSnapshottedTransform() {
    EnumMap<ModelPartType, EasyModelPartTransform> transforms =
        new EnumMap<>(ModelPartType.class);
    EasyModelPartTransform expected =
        new EasyModelPartTransform(10.0f, 20.0f, 30.0f)
            .withOffset(1.0f, 2.0f, 3.0f)
            .withScale(1.1f, 1.2f, 1.3f)
            .withVisible(false);
    transforms.put(ModelPartType.HEAD, expected);
    EasyModelPartAnimator animator = EasyModelNPCPartAnimator.snapshot(transforms);

    transforms.clear();

    assertEquals(expected, animator.animate(context("head")));
    assertEquals(EasyModelPartTransform.NONE, animator.animate(context("custom_bone")));
  }
}
