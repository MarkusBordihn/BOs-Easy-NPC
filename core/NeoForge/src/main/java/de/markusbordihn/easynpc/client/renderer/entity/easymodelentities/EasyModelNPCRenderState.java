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

import de.markusbordihn.easymodelentities.api.data.client.EasyModelItemAnchor;
import de.markusbordihn.easymodelentities.client.render.EasyModelEntityRenderState;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCGuiRenderStateExtension;
import net.minecraft.client.renderer.item.ItemStackRenderState;
import net.minecraft.resources.Identifier;

@SuppressWarnings("java:S1104")
public class EasyModelNPCRenderState extends EasyModelEntityRenderState
    implements EasyNPCGuiRenderStateExtension {

  public final ItemStackRenderState mainHandItem = new ItemStackRenderState();
  public final ItemStackRenderState offHandItem = new ItemStackRenderState();
  public Identifier profileId;
  public float rootScaleX = 1.0f;
  public float rootScaleY = 1.0f;
  public float rootScaleZ = 1.0f;
  public float previewScale;
  public float previewYLift;
  public float rootRotationX;
  public float rootRotationZ;
  public float rootPivotY;
  public EasyModelItemAnchor mainHandAnchor;
  public EasyModelItemAnchor offHandAnchor;
  public boolean mainArmLeft;

  @Override
  public void applyGuiRotationsAndScale(float xRotation, float yRotation) {
    this.entityYaw = 180.0f + xRotation * 20.0f;
  }
}
