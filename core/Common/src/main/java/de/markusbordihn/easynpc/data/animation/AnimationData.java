/*
 * Copyright 2024 Markus Bordihn
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

package de.markusbordihn.easynpc.data.animation;

import java.util.List;
import java.util.Map;

public class AnimationData {

  private String format_version;
  private Map<String, Animation> animations;

  public String getFormatVersion() {
    return format_version;
  }

  public void setFormatVersion(String format_version) {
    this.format_version = format_version;
  }

  public Map<String, Animation> getAnimations() {
    return animations;
  }

  public void setAnimations(Map<String, Animation> animations) {
    this.animations = animations;
  }

  @Override
  public String toString() {
    return "AnimationData{"
        + "format_version='"
        + format_version
        + '\''
        + ", animations="
        + animations
        + '}';
  }

  public static class Animation {
    private String name; // Name der Animation
    private String loop;
    private Float animation_length;
    private Map<String, Bone> bones;

    public String getName() {
      return name;
    }

    public void setName(String name) {
      this.name = name;
    }

    public String getLoop() {
      return loop;
    }

    public void setLoop(String loop) {
      this.loop = loop;
    }

    public Float getAnimationLength() {
      return animation_length;
    }

    public void setAnimationLength(Float animation_length) {
      this.animation_length = animation_length;
    }

    public Map<String, Bone> getBones() {
      return bones;
    }

    public void setBones(Map<String, Bone> bones) {
      this.bones = bones;
    }

    @Override
    public String toString() {
      return "Animation{"
          + "name='"
          + name
          + '\''
          + ", loop='"
          + loop
          + '\''
          + ", animation_length="
          + animation_length
          + ", bones="
          + bones
          + '}';
    }
  }

  public static class Bone {
    private List<Float> position; // Für Arrays wie [0, 0, 0]
    private List<Float> rotation; // Für Arrays wie [0, 0, 0]
    private Float scale; // Optional
    private Map<String, List<Float>> keyframePosition; // Für Keyframes bei Position
    private Map<String, List<Float>> keyframeRotation; // Für Keyframes bei Rotation

    public List<Float> getPosition() {
      return position;
    }

    public void setPosition(List<Float> position) {
      this.position = position;
    }

    public List<Float> getRotation() {
      return rotation;
    }

    public void setRotation(List<Float> rotation) {
      this.rotation = rotation;
    }

    public Float getScale() {
      return scale;
    }

    public void setScale(Float scale) {
      this.scale = scale;
    }

    public Map<String, List<Float>> getKeyframePosition() {
      return keyframePosition;
    }

    public void setKeyframePosition(Map<String, List<Float>> keyframePosition) {
      this.keyframePosition = keyframePosition;
    }

    public Map<String, List<Float>> getKeyframeRotation() {
      return keyframeRotation;
    }

    public void setKeyframeRotation(Map<String, List<Float>> keyframeRotation) {
      this.keyframeRotation = keyframeRotation;
    }

    @Override
    public String toString() {
      return "Bone{"
          + "position="
          + position
          + ", rotation="
          + rotation
          + ", scale="
          + scale
          + ", keyframePosition="
          + keyframePosition
          + ", keyframeRotation="
          + keyframeRotation
          + '}';
    }
  }
}
