import React from 'react';
import PropTypes from 'prop-types';
import { drawOps } from './../Canvas.fs';
import ReactAnimationFrame from 'react-animation-frame';

class Canvas extends React.Component {

    constructor(props) {
        super(props);
        this.lastFrameTimeMs = 0;
        this.frameId = 0;
    }

    componentDidMount() {
        // drawOps(this.getContext(), this.props.drawOps);
        // this.refs.canvas.addEventListener("mousemove", this.mouveMove, false);
        requestAnimationFrame(this.onFrame);
        // console.log("Mount:", performance.now());
    }

    onFrame = timestamp => {
        if (timestamp < this.lastFrameTimeMs + (1000 / this.props.maxFPS)) {
            requestAnimationFrame(this.onFrame);
            return;
        }

        let delta = timestamp - this.lastFrameTimeMs;
        this.lastFrameTimeMs = timestamp;
        const timestep = 1000/this.props.maxFPS;
        // console.log("onFrame", this.frameId, " | Steps: ", delta / timestep );

        while (delta >= timestep) {
            this.props.onTick([timestep, 10.]);
            delta -= timestep;
        }
        this.frameId += 1;

        // console.log(this.props.onTick());
        // console.log("OnFrame_end:", performance.now());
        // requestAnimationFrame(this.onFrame);
    }

    componentDidUpdate() {
        // if (this.props.isPlaying) {
            // drawOps(this.getContext(), this.props.drawOps);
            // For now we can tick directly because we are using
            // `Program.withReact` which use RAF internally
            // Would be nice if we could use RAF here directly for more control perhaps ?
            // this.props.onTick();
        // }
        // drawOps(this.getContext(), this.props.drawOps);
        // console.log("DidUpdate", this.frameId);
        drawOps(this.getContext(), this.props.drawOps);
        // console.log("DidUpdate:", performance.now());
        requestAnimationFrame(this.onFrame);
    }

    getContext() {
        return this.refs.canvas.getContext("2d");
    }

    render() {
        return (
            <canvas width={this.props.width}
                onMouseMove={this.props.onMouseMove}
                height={this.props.height}
                style={this.props.style}
                ref="canvas">

            </canvas>
        );
    }
}

// export default ReactAnimationFrame(Canvas);

export default Canvas;
